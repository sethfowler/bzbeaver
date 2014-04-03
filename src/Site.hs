{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader (asks)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Acid as AS
import           Data.ByteString (ByteString)
import           Data.Default (def)
import           Data.List (groupBy)
import           Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import qualified Network.Gravatar as G
import           Snap
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.AcidState (Update, Query, update, query, acidInitManual)
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Heist
import qualified Heist.Interpreted as I
import qualified Text.XmlHtml as X
import           Web.Bugzilla
------------------------------------------------------------------------------
import           Application
import           Bugzilla


------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = maybe noSplices splice authError
    splice err = "loginError" ## I.textSplice err


------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit = do
    mUser <- getParam "login"
    mPass <- getParam "password"
    case (mUser, mPass) of
      (Just user, Just pass) -> loginUser "login" "password" Nothing
                                          (\_ -> handleLogin err)
                                          (loginSuccess user pass)
      _                      -> handleLogin invalidErr
  where
    err = Just "Unknown user or password."
    invalidErr = Just "Invalid request. Try again."

    loginSuccess user pass = do
      let userText = TE.decodeUtf8 user
          passText = TE.decodeUtf8 pass

      -- Create a Bugzilla session for this user.
      session <- liftIO $ newBzSession userText (Just passText)
      mvSessions <- withTop' id $ gets $ _bzSessions
      liftIO $ modifyMVar_ mvSessions $
        return . M.insert userText session

      -- Synchronously update requests if there are none stored.
      mReqs <- query $ GetRequests userText
      when (isNothing mReqs) $ do
        reqs <- liftIO $ bzRequests session userText
        update $ UpdateRequests userText reqs

      -- We're now ready to display the dashboard.
      redirect "/dashboard"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"


------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = do
      mUser <- getParam "login"
      case mUser of
        (Just user) -> do let newTime = posixSecondsToUTCTime 0
                              userText = TE.decodeUtf8 user
                          update $ AddUser userText (UserInfo newTime)
                          registerUser "login" "password"
                          redirect "/"
        _           -> redirect "/new_user"


------------------------------------------------------------------------------
-- | Handle dashboard
handleDashboard :: Handler App (AuthManager App) ()
handleDashboard = do
  mUser <- currentUser
  case mUser of
    Just user -> do
      let login = userLogin user
      curTime <- liftIO $ getCurrentTime
      mUserInfo <- query $ GetUser login
      update $ UpdateUser login (UserInfo curTime)
      mReqs <- query $ GetRequests login
      case (mUserInfo, mReqs) of
        (Just ui, Just reqs) -> renderDashboard curTime (ui ^. uiLastSeen) reqs
        (Nothing, _)         -> handleLogin $ Just "You don't exist."
        _                    -> handleLogin $ Just "You've been logged out. Please log in again."
    Nothing -> handleLogin $ Just "You need to be logged in for that."

renderDashboard :: UTCTime -> UTCTime -> [BzRequest] -> Handler App (AuthManager App) ()
renderDashboard curTime lastSeen requests = do
  let (nis, rs, fs, as, ps, rds) = countRequests requests
      splices = do "dashboardItems" ## I.mapSplices (dashboardItemSplice lastSeen curTime)
                                                    requests
                   "needinfoCount"  ## (return [X.TextNode . T.pack . show $ nis])
                   "reviewCount"    ## (return [X.TextNode . T.pack . show $ rs])
                   "feedbackCount"  ## (return [X.TextNode . T.pack . show $ fs])
                   "assignedCount"  ## (return [X.TextNode . T.pack . show $ as])
                   "pendingCount"   ## (return [X.TextNode . T.pack . show $ ps])
                   "reviewedCount"  ## (return [X.TextNode . T.pack . show $ rds])
  heistLocal (I.bindSplices splices) $ render "dashboard"

dashboardItemSplice :: UTCTime -> UTCTime -> BzRequest -> I.Splice (Handler App App)
dashboardItemSplice lastSeen curTime req = return $
    [divNode ["item", containerClass req] $
       [ divNode [reqClass req] $
           titleBadge req ++
             [X.Element "h1" [classes ["card-title", reqClass req]] [title req]]
       ] ++ extended req ++ [
         divNode ["summary"] [X.Element "p" [] [bugLink req]]
       ] ++ details req ++
       [ lastComment req
       , comments req
       ]
    ]

  where

    containerClass (NeedinfoRequest _ _ _) = "needinfo-container"
    containerClass (ReviewRequest _ _ _)   = "review-container"
    containerClass (FeedbackRequest _ _ _) = "feedback-container"
    containerClass (AssignedRequest _ _ _)   = "assigned-container"
    containerClass (PendingRequest _ _ _)   = "pending-container"
    containerClass (ReviewedRequest _ _ _)   = "reviewed-container"

    title r = linkNode (url r) [T.toUpper . reqClass $ r]
      where
        url (NeedinfoRequest _ _ _)   = bugURL r
        url (ReviewRequest _ _ att)   = attURL r att
        url (FeedbackRequest _ _ att) = attURL r att
        url (AssignedRequest _ _ _)   = bugURL r
        url (PendingRequest _ _ _)    = bugURL r
        url (ReviewedRequest _ _ _)   = bugURL r

    titleBadge (NeedinfoRequest _ cs flag) = timeBadgeNode lastSeen cs curTime
                                             (flagCreationDate flag)
    titleBadge (ReviewRequest _ cs att)    = timeBadgeNode lastSeen cs curTime
                                             (attachmentCreationTime att)
    titleBadge (FeedbackRequest _ cs att)  = timeBadgeNode lastSeen cs curTime
                                             (attachmentCreationTime att)
    titleBadge (AssignedRequest bug cs _)  = timeBadgeNode lastSeen cs curTime
                                             (bugCreationTime bug)
    titleBadge (PendingRequest bug cs _)   = timeBadgeNode lastSeen cs curTime
                                             (bugCreationTime bug)
    titleBadge (ReviewedRequest bug cs _)   = timeBadgeNode lastSeen cs curTime
                                              (bugCreationTime bug)

    reqClass (NeedinfoRequest _ _ _) = "needinfo"
    reqClass (ReviewRequest _ _ _)   = "review"
    reqClass (FeedbackRequest _ _ _) = "feedback"
    reqClass (AssignedRequest _ _ _) = "assigned"
    reqClass (PendingRequest _ _ _)  = "pending"
    reqClass (ReviewedRequest _ _ _) = "reviewed"

    bugLink r = linkNode (bugURL r) [bugIdText r, ": ", bugSummary . reqBug $ r]
            
    details (NeedinfoRequest bug _ flag) = []
    details (ReviewRequest bug _ att)    = [ divNode ["detail"] (attachmentNode att)
                                           ]
    details (FeedbackRequest bug _ att)  = [ divNode ["detail"] (attachmentNode att)
                                           ]
    details (AssignedRequest bug _ atts) = [ divNode ["detail"] (concatMap attachmentNode atts)
                                           ]
    details (PendingRequest bug _ atts)  = [ divNode ["detail"] (concatMap attachmentNode atts)
                                           ]
    details (ReviewedRequest bug _ atts) = [ divNode ["detail"] (concatMap attachmentNode atts)
                                           ]

    attachmentNode att =
      [ X.Element "hr" [] []
      , divNode ["attachment"] $
          [ divNode ["attachment-icon"] []
          , pNode' ["attachment-summary"] [attachmentSummary att]
          ] ++ map attachmentFlagNode (attachmentFlags att)
      ]


    attachmentFlagNode f = divNode ["attachment-flag"] $
      [ maybe noSmallGravatar smallGravatarImg (flagRequestee f)
      , X.Element "p" [] $
          [ flagNameNode [flagName f, flagStatus f]
          , textNode [" " , fromMaybe "" (flagRequestee f)]
          ]
      ]
      
    extended (NeedinfoRequest bug _ flag) = [ divNode ["extended"]
      [ gravatarImg (flagSetter flag)
      , pNode [flagSetter flag]
      , timeNode curTime (flagCreationDate flag)
      ] ]
    extended (ReviewRequest bug _ att) = [ divNode ["extended"]
      [ gravatarImg (attachmentCreator att)
      , pNode [attachmentCreator att]
      , timeNode curTime (attachmentCreationTime att)
      ] ]
    extended (FeedbackRequest bug _ att) = [ divNode ["extended"]
      [ gravatarImg (attachmentCreator att)
      , pNode [attachmentCreator att]
      , timeNode curTime (attachmentCreationTime att)
      ] ]
    extended (AssignedRequest bug _ _) = []
    extended (PendingRequest bug _ _)  = []
    extended (ReviewedRequest bug _ _) = []

    lastComment r = divNode ["last-comment-wrapper"] $
                      map (commentNode r 200) (take 1 . reqComments $ r)

    comments r = divNode ["comments-wrapper"] $
                   [ divNode ["comments-inner-wrapper"] $
                     [ divNode ["comments"] $ map (commentNode r 0) (reqComments r)]]

    commentNode r limit c = divNode ["comment"] $
      [ divNode (commentHeaderClasses lastSeen $ commentCreationTime c) $
          [ gravatarImg (commentCreator c)
          , pNode [commentCreator c]
          , X.Element "p" [] $
            [ linkNode (commentURL r c) ["#", T.pack . show $ commentCount c] 
            , textNode [" (", T.pack . show $ commentCreationTime c, ")"]
            ]
          ]
      ] ++ (mergedComments . T.lines . applyLimit limit . commentText $ c)

    mergedComments = concatMap mergeQuotes . groupBy (same numQuotes)
      where
        mergeQuotes [] = []
        mergeQuotes ls@(l:_)
          | isQuote l = if any (";" `T.isSuffixOf`) ls
                          then map (pQuoteNode . (:[])) ls
                          else [pQuoteNode [T.intercalate " " . map stripQuotes $ ls]]
          | otherwise = map (pNode . (:[])) ls

    commentTextNode line
      | ">" `T.isPrefixOf` line = X.Element "p" [classes ["quote"]] [X.TextNode line]
      | otherwise               = pNode [line]

    gravatarImg email = divNode ["gravatar"] [X.Element "img" [("src", url)] []]
      where
        url = T.pack $ G.gravatar settings email
        settings = def { G.gDefault = Just G.Retro, G.gSize = Just $ G.Size 48 }

    smallGravatarImg email = divNode ["small-gravatar"] [X.Element "img" [("src", url)] []]
      where
        url = T.pack $ G.gravatar settings email
        settings = def { G.gDefault = Just G.Retro, G.gSize = Just $ G.Size 32 }

    noGravatar = divNode ["no-gravatar"] []
    noSmallGravatar = divNode ["no-small-gravatar"] []

attURL :: BzRequest -> Attachment -> T.Text
attURL r att = T.concat [ "https://bugzilla.mozilla.org/page.cgi?id=splinter.html&bug="
                        , bugIdText r
                        , "&attachment="
                        , attachmentIdText att
                        ]

bugURL :: BzRequest -> T.Text
bugURL r = T.concat ["https://bugzilla.mozilla.org/show_bug.cgi?id=", bugIdText r]

commentURL :: BzRequest -> Comment -> T.Text
commentURL r c = T.concat [ "https://bugzilla.mozilla.org/show_bug.cgi?id="
                          , bugIdText r
                          , "#c"
                          , commentIdText c
                          ]
                 
attachmentIdText :: Attachment -> T.Text
attachmentIdText = T.pack . show . attachmentId

bugIdText :: BzRequest -> T.Text
bugIdText = T.pack . show . bugId . reqBug

commentIdText :: Comment -> T.Text
commentIdText = T.pack . show . commentCount

classes :: [T.Text] -> (T.Text, T.Text)
classes cs = ("class", T.intercalate " " cs)

textNode :: [T.Text] -> X.Node
textNode = X.TextNode . T.concat

linkNode :: T.Text -> [T.Text] -> X.Node
linkNode url ts = X.Element "a" [("href", url)] [textNode ts]

pNode :: [T.Text] -> X.Node
pNode ts = X.Element "p" [] [textNode ts]

flagNameNode :: [T.Text] -> X.Node
flagNameNode ts = X.Element "span" [classes ["flag-name"]] [textNode ts]

pNode' :: [T.Text] -> [T.Text] -> X.Node
pNode' cs ts = X.Element "p" [classes cs] [textNode ts]

pQuoteNode :: [T.Text] -> X.Node
pQuoteNode ts = X.Element "p" [classes ["quote"]] [textNode ts]

isQuote :: T.Text -> Bool
isQuote = (">" `T.isPrefixOf`)

divNode :: [T.Text] -> [X.Node] -> X.Node
divNode cs es = X.Element "div" [classes cs] es

oneWeek, twoWeeks, oneMonth :: NominalDiffTime
oneWeek  = fromIntegral $ 1 * 604800
twoWeeks = fromIntegral $ 2 * 604800
oneMonth = fromIntegral $ 4 * 604800 

timeNode :: UTCTime -> UTCTime -> X.Node
timeNode curTime itemTime
  | timeDiff > oneWeek  = node ["overdue"]
  | otherwise           = node []
  where
    timeDiff = curTime `diffUTCTime` itemTime
    node cs = X.Element "p" [classes cs]
                            [textNode [T.pack . show $ itemTime]]

timeBadgeNode :: UTCTime -> [Comment] -> UTCTime -> UTCTime -> [X.Node]
timeBadgeNode lastSeen cs curTime itemTime = newComment cs ++ overdue
  where
    newComment (c:_)
      | commentTimeDiff > 0 = [divNode ["left-badge"] [textNode ["+"]]]
      | otherwise           = []
      where
        commentTimeDiff = (commentCreationTime c) `diffUTCTime` lastSeen

    newComment _ = []

    overdue
      | overdueTimeDiff > oneMonth = [overdueNode ["!!!"]]
      | overdueTimeDiff > twoWeeks = [overdueNode ["!!"]]
      | overdueTimeDiff > oneWeek  = [overdueNode ["!"]]
      | otherwise                  = []
      where
        overdueTimeDiff = curTime `diffUTCTime` itemTime
        overdueNode ts  = divNode ["right-badge"] [textNode ts]

commentHeaderClasses :: UTCTime -> UTCTime -> [T.Text]
commentHeaderClasses lastSeen itemTime
  | timeDiff > 0 = ["comment-header", "new-comment"]
  | otherwise    = ["comment-header"]
  where
    timeDiff = itemTime `diffUTCTime` lastSeen

same :: Eq b => (a -> b) -> a -> a -> Bool
same f a b = f a == f b

numQuotes :: T.Text -> Int
numQuotes = length . takeWhile (== "> ") . T.chunksOf 2

stripQuotes :: T.Text -> T.Text
stripQuotes = T.concat . dropWhile (== "> ") . T.chunksOf 2

applyLimit :: Int -> T.Text -> T.Text
applyLimit 0 = id
applyLimit n = (`T.append` "...") . T.take n

countRequests :: [BzRequest] -> (Int, Int, Int, Int, Int, Int)
countRequests rs = go (0, 0, 0, 0, 0, 0) rs
  where
    go !count [] = count
    go (!nis, !rvs, !fbs, !as, !ps, !rds) ((NeedinfoRequest _ _ _) : rs) = go (nis + 1, rvs, fbs, as, ps, rds) rs
    go (!nis, !rvs, !fbs, !as, !ps, !rds) ((ReviewRequest _ _ _) : rs)   = go (nis, rvs + 1, fbs, as, ps, rds) rs
    go (!nis, !rvs, !fbs, !as, !ps, !rds) ((FeedbackRequest _ _ _) : rs) = go (nis, rvs, fbs + 1, as, ps, rds) rs
    go (!nis, !rvs, !fbs, !as, !ps, !rds) ((AssignedRequest _ _ _) : rs) = go (nis, rvs, fbs, as + 1, ps, rds) rs
    go (!nis, !rvs, !fbs, !as, !ps, !rds) ((PendingRequest _ _ _) : rs)  = go (nis, rvs, fbs, as, ps + 1, rds) rs
    go (!nis, !rvs, !fbs, !as, !ps, !rds) ((ReviewedRequest _ _ _) : rs) = go (nis, rvs, fbs, as, ps, rds + 1) rs

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/login",     with auth handleLoginSubmit)
         , ("/logout",    with auth handleLogout)
         , ("/new_user",  with auth handleNewUser)
         , ("/dashboard", with auth handleDashboard)
         , ("",           serveDirectory "static")
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    -- NOTE: We're using initJsonFileAuthManager here because it's easy and
    -- doesn't require any kind of database server to run.  In practice,
    -- you'll probably want to change this to a more robust auth backend.
    a <- nestSnaplet "auth" auth $
           initJsonFileAuthManager defAuthSettings sess "users.json"

    state <- liftIO $ AS.openLocalState (AppState M.empty M.empty)
    onUnload (AS.closeAcidState state)
    acid <- nestSnaplet "acid" acid $ acidInitManual state

    addRoutes routes
    addAuthSplices h auth

    mvSessions <- liftIO $ newMVar M.empty
    t <- liftIO $ async $ pollBugzilla mvSessions state 15

    return $ App h s a acid t mvSessions

pollBugzilla :: MVar (M.Map UserLogin BugzillaSession) -> AS.AcidState AppState -> Int -> IO ()
pollBugzilla !mvSessions !state !intervalMin = do
  -- Wait appropriately.
  let intervalUs = 60000000 * intervalMin
  threadDelay intervalUs

  -- Update all users with current sessions.
  sessions <- readMVar mvSessions
  forM_ (M.toList sessions) $ \(user, session) -> do
    putStrLn $ "Updating requests from Bugzilla for user " ++ show user
    reqs <- bzRequests session user
    AS.update state $ UpdateRequests user reqs

  -- Do it again.
  pollBugzilla mvSessions state intervalMin
