{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Control.Monad.Reader (asks)
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Lens
import qualified Data.Map as M
import Data.SafeCopy (base, deriveSafeCopy)
import qualified Data.Text as T
import Data.Time.Clock
import Data.Typeable
import Snap (modify)
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
import Snap.Snaplet.AcidState (Update, Query, Acid, HasAcid (getAcidStore),
                               makeAcidic, update, query, acidInit)
import Bugzilla

------------------------------------------------------------------------------
-- | Persistent state.

type UserLogin    = T.Text
type UserPassword = T.Text

data UserInfo = UserInfo
  { _uiLastSeen :: UTCTime
  } deriving (Eq, Ord, Show, Typeable)

makeLenses ''UserInfo
deriveSafeCopy 0 'base ''UserInfo

data AppState = AppState
  { _asUsers    :: M.Map UserLogin UserInfo
  , _asRequests :: M.Map UserLogin [BzRequest]
  } deriving (Typeable)

makeLenses ''AppState
deriveSafeCopy 0 'base ''AppState

updateUser :: UserLogin -> UserInfo -> Update AppState ()
updateUser login info = modify $ over asUsers (M.insert login info)

-- The only difference between 'addUser' and 'updateUser' is that
-- 'addUser' doesn't update 'uiLastSeen'.
addUser :: UserLogin -> UserInfo -> Update AppState ()
addUser login info = modify $ over asUsers (M.insertWith merge login info)
  where merge _ old = old 

getUser :: UserLogin -> Query AppState (Maybe UserInfo)
getUser login = asks $ M.lookup login . (^. asUsers)

updateRequests :: UserLogin -> [BzRequest] -> Update AppState ()
updateRequests login reqs = modify $ over asRequests (M.insert login reqs)

getRequests :: UserLogin -> Query AppState (Maybe [BzRequest])
getRequests login = asks $ M.lookup login . (^. asRequests)

makeAcidic ''AppState ['updateUser, 'addUser, 'getUser, 'updateRequests, 'getRequests]

------------------------------------------------------------------------------
data App = App
    { _heist      :: Snaplet (Heist App)
    , _sess       :: Snaplet SessionManager
    , _auth       :: Snaplet (AuthManager App)
    , _acid       :: Snaplet (Acid AppState)
    , _bzThread   :: Async ()
    , _bzSessions :: MVar (M.Map UserLogin BugzillaSession)
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasAcid App AppState where
    getAcidStore = view (acid.snapletValue)

------------------------------------------------------------------------------
type AppHandler = Handler App App


