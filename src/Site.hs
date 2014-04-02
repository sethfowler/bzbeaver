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
import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString (ByteString)
import           Data.Default (def)
import           Data.Maybe
import qualified Data.Text as T
import           Data.Time.Clock
import qualified Network.Gravatar as G
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
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/dashboard")
  where
    err = Just "Unknown user or password"


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
      curTime <- liftIO $ getCurrentTime
      requests <- liftIO $ bzRequests login Nothing
      let splices = "dashboardItems"
                 ## I.mapSplices (dashboardItemSplice curTime) requests
      heistLocal (I.bindSplices splices) $ render "dashboard"
    Nothing -> handleLogin $ Just "You need to be logged in for that."

dashboardItemSplice :: UTCTime -> BzRequest -> I.Splice (Handler App App)
dashboardItemSplice curTime req = return $
    [divNode ["item"] $
       [ divNode [reqClass req] $
         [X.Element "h1" [classes ["card-title", reqClass req]] [title req]]
       , divNode ["summary"] [X.Element "p" [] [bugLink req]]
       , X.Element "hr" [] []
       , details req
       , extended req
       ]
    ]
  where
    title r = X.Element "a" [("href", url r)] [X.TextNode . T.toUpper . reqClass $ r]
      where
        url (NeedinfoRequest _ _) = bugURL r
        url (ReviewRequest _ att) = attURL r att
        url (FeedbackRequest _ att) = attURL r att

    reqClass (NeedinfoRequest _ _) = "needinfo"
    reqClass (ReviewRequest _ _)   = "review"
    reqClass (FeedbackRequest _ _) = "feedback"

    bugLink r = X.Element "a" [("href", url)] [ X.TextNode $ bugIdText r
                                              , X.TextNode ": "
                                              , X.TextNode . bugSummary . reqBug $ r
                                              ]
      where
        url = T.concat [bugURL r]
            
    details (NeedinfoRequest bug flag) = divNode ["detail"] []
    details (ReviewRequest bug att) = divNode ["detail"] [pNode [attachmentSummary att]]
    details (FeedbackRequest bug att) = divNode ["detail"] [pNode [attachmentSummary att]]


    extended (NeedinfoRequest bug flag) = divNode ["extended"] $
      [ gravatarImg (flagSetter flag)
      , pNode [flagSetter flag]
      , timeNode curTime (flagCreationDate flag)
      ]
    extended (ReviewRequest bug att) = divNode ["extended"] $
      [ gravatarImg (attachmentCreator att)
      , pNode [attachmentCreator att]
      , timeNode curTime (attachmentCreationTime att)
      ]
    extended (FeedbackRequest bug att) = divNode ["extended"] $
      [ gravatarImg (attachmentCreator att)
      , pNode [attachmentCreator att]
      , timeNode curTime (attachmentCreationTime att)
      ]

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

reqBug :: BzRequest -> Bug
reqBug (NeedinfoRequest bug _) = bug
reqBug (ReviewRequest bug _)   = bug
reqBug (FeedbackRequest bug _) = bug

attachmentIdText :: Attachment -> T.Text
attachmentIdText = T.pack . show . attachmentId

bugIdText :: BzRequest -> T.Text
bugIdText = T.pack . show . bugId . reqBug

classes :: [T.Text] -> (T.Text, T.Text)
classes cs = ("class", T.intercalate " " cs)

textNode :: [T.Text] -> X.Node
textNode = X.TextNode . T.concat

pNode :: [T.Text] -> X.Node
pNode ts = X.Element "p" [] [textNode ts]

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
    return $ App h s a
