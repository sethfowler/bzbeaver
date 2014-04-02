{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Lens
import qualified Data.Map as M
import qualified Data.Text as T
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Session

import Bugzilla

------------------------------------------------------------------------------
data UserInfo = UserInfo T.Text T.Text
                deriving (Eq, Ord, Show)

data App = App
    { _heist :: Snaplet (Heist App)
    , _sess :: Snaplet SessionManager
    , _auth :: Snaplet (AuthManager App)
    , _bzThread :: Async ()
    , _bzRequestsMVar :: MVar (M.Map UserInfo [BzRequest])
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist


------------------------------------------------------------------------------
type AppHandler = Handler App App


