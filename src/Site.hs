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
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString (ByteString)
import           Data.Default (def)
import           Data.List (groupBy)
import           Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time.Clock
import qualified Network.Gravatar as G
import           Snap
import           Snap.Core
import           Snap.Snaplet
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
      reqMVar <- withTop' id $ gets $ _bzRequestsMVar
      liftIO $ addPolledBugzillaRequest reqMVar (TE.decodeUtf8 user) (TE.decodeUtf8 pass)

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
    handleFormSubmit = registerUser "login" "password" >> redirect "/"


------------------------------------------------------------------------------
-- | Handle dashboard
handleDashboard :: Handler App (AuthManager App) ()
handleDashboard = do
  mUser <- currentUser
  case mUser of
    Just user -> do
      let login = userLogin user
      reqMVar <- withTop' id $ gets $ _bzRequestsMVar
      mRequests <- liftIO $ getBugzillaRequest reqMVar login
      case mRequests of
        Just requests -> do curTime <- liftIO $ getCurrentTime
                            let splices = "dashboardItems"
                                          ## I.mapSplices (dashboardItemSplice curTime) requests
                            heistLocal (I.bindSplices splices) $ render "dashboard"
        Nothing -> handleLogin $ Just "You've been logged out. Please log in again."
    Nothing -> handleLogin $ Just "You need to be logged in for that."

dashboardItemSplice :: UTCTime -> BzRequest -> I.Splice (Handler App App)
dashboardItemSplice curTime req = return $
    [divNode ["item"] $
       [ divNode [reqClass req] $
         [X.Element "h1" [classes ["card-title", reqClass req]] [title req]]
       , extended req
       ] ++ details req ++
       [ divNode ["summary"] [X.Element "p" [] [bugLink req]]
       , lastComment req
       , comments req
       ]
    ]
  where
    title r = linkNode (url r) [T.toUpper . reqClass $ r]
      where
        url (NeedinfoRequest _ _ _) = bugURL r
        url (ReviewRequest _ _ att) = attURL r att
        url (FeedbackRequest _ _ att) = attURL r att

    reqClass (NeedinfoRequest _ _ _) = "needinfo"
    reqClass (ReviewRequest _ _ _)   = "review"
    reqClass (FeedbackRequest _ _ _) = "feedback"

    bugLink r = linkNode (bugURL r) [bugIdText r, ": ", bugSummary . reqBug $ r]
            
    details (NeedinfoRequest bug _ flag) = []
    details (ReviewRequest bug _ att) = [ divNode ["detail"] [pNode [attachmentSummary att]]
                                        , X.Element "hr" [] []
                                        ]
    details (FeedbackRequest bug _ att) = [ divNode ["detail"] [pNode [attachmentSummary att]]
                                          , X.Element "hr" [] []
                                          ]


    extended (NeedinfoRequest bug _ flag) = divNode ["extended"] $
      [ gravatarImg (flagSetter flag)
      , pNode [flagSetter flag]
      , timeNode curTime (flagCreationDate flag)
      ]
    extended (ReviewRequest bug _ att) = divNode ["extended"] $
      [ gravatarImg (attachmentCreator att)
      , pNode [attachmentCreator att]
      , timeNode curTime (attachmentCreationTime att)
      ]
    extended (FeedbackRequest bug _ att) = divNode ["extended"] $
      [ gravatarImg (attachmentCreator att)
      , pNode [attachmentCreator att]
      , timeNode curTime (attachmentCreationTime att)
      ]

    lastComment r = divNode ["last-comment-wrapper"] $
                      map (commentNode r 200) (take 1 . reqComments $ r)

    comments r = divNode ["comments-wrapper"] $
                   [ divNode ["comments-inner-wrapper"] $
                     [ divNode ["comments"] $ map (commentNode r 0) (reqComments r)]]

    commentNode r limit c = divNode ["comment"] $
      [ divNode ["comment-header"] $
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
  | timeDiff > oneMonth = node ["extremely-overdue"]
  | timeDiff > twoWeeks = node ["very-overdue"]
  | timeDiff > oneWeek  = node ["overdue"]
  | otherwise           = node []
  where
    timeDiff = curTime `diffUTCTime` itemTime
    node cs = X.Element "p" [classes cs]
                            [textNode [T.pack . show $ itemTime]]

same :: Eq b => (a -> b) -> a -> a -> Bool
same f a b = f a == f b

numQuotes :: T.Text -> Int
numQuotes = length . takeWhile (== "> ") . T.chunksOf 2

stripQuotes :: T.Text -> T.Text
stripQuotes = T.concat . dropWhile (== "> ") . T.chunksOf 2

applyLimit :: Int -> T.Text -> T.Text
applyLimit 0 = id
applyLimit n = (`T.append` "...") . T.take n

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
    addRoutes routes
    addAuthSplices h auth

    mv <- liftIO $ newMVar M.empty
    t <- liftIO $ async $ pollBugzilla mv 15

    return $ App h s a t mv

pollBugzilla :: MVar (M.Map UserInfo [BzRequest]) -> Int -> IO ()
pollBugzilla !mv !intervalMin = do
  -- Wait appropriately.
  let intervalUs = 60000000 * intervalMin
  threadDelay intervalUs

  -- Update all registered users.
  modifyMVar_ mv $ \bzReqs -> do

    newReqs <- forM (M.toList bzReqs) $ \(ui@(UserInfo user pass), _) -> do
      rs <- bzRequests user (Just pass)
      return (ui, rs)

    return $ M.fromList newReqs

  -- Do it again.
  pollBugzilla mv intervalMin

addPolledBugzillaRequest :: MVar (M.Map UserInfo [BzRequest]) -> T.Text -> T.Text -> IO ()
addPolledBugzillaRequest mv user pass = do
  -- Perform an initial request.
  reqs <- bzRequests user (Just pass)

  -- Update the registered requests map.
  modifyMVar_ mv $ \bzReqs ->
    return $ M.insert (UserInfo user pass) reqs bzReqs

getBugzillaRequest :: MVar (M.Map UserInfo [BzRequest]) -> T.Text -> IO (Maybe [BzRequest])
getBugzillaRequest mv user = do
  bzReqs <- readMVar mv
  let userReqs = filter (\((UserInfo u p), v) -> u == user) . M.toList $ bzReqs
  case userReqs of
    [(_, req)] -> return $ Just req
    _          -> return Nothing
