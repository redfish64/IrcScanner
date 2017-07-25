{-# LANGUAGE OverloadedStrings #-}
module IrcScanner.KeywordIndexPage where

import IrcScanner.Types
import           Snap
import           Snap.Snaplet.Heist
import           Control.Lens
import Control.Monad.IO.Class(liftIO)
--import Control.Monad.Trans(lift)
import Control.Monad.Reader(ask)
import Data.IORef(readIORef)
import Heist.Interpreted
import Heist
import Data.Map.Syntax((##))
import Data.Text(pack)
-- import Control.Monad.Trans.Either (runEitherT,left, EitherT(..))
-- import Control.Monad.Trans (lift)
-- import Data.ByteString(ByteString)
-- import Data.Text.Encoding

splicesFromCIR :: Monad n => CachedIndexResult -> Splices (Splice n)
splicesFromCIR cir =
  do
    "keyword" ## textSplice $ view (cindex . idisplayName) cir

allSplices :: [CachedIndexResult] -> Integer -> Splices (SnapletISplice x)
allSplices cirs ht =
  do
    "allIndexes" ## (mapSplices (runChildrenWith . splicesFromCIR) cirs)
    "hacktest" ## textSplice $ pack $ show ht


keywordIndexHandler :: HasHeist x => Handler x IrcSnaplet ()
keywordIndexHandler = do
  modifySnapletState $ over (snapletValue . hacktest) (+1)
  s <- ask
  st <- liftIO $ readIORef (view (iconfig . cstate) s)
  
  renderWithSplices "index" (allSplices (_scirs st) (_hacktest s))
