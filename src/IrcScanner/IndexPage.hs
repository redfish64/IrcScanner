module IndexPage where

import Types
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
import Data.Text(append)
-- import Control.Monad.Trans.Either (runEitherT,left, EitherT(..))
-- import Control.Monad.Trans (lift)
-- import Data.ByteString(ByteString)
-- import Data.Text.Encoding

splicesFromCIR :: Monad n => CachedIndexResult -> Splices (Splice n)
splicesFromCIR cir =
  do
    "keywordURL" ## (textSplice $ "lookup?name=" `append` view (cindex . idisplayName) cir)
    "keyword" ## textSplice $ view (cindex . idisplayName) cir

allSplices :: [CachedIndexResult] -> Splices (SnapletISplice IrcSnaplet)
allSplices cirs = "allIndexes" ## (mapSplices (runChildrenWith . splicesFromCIR) cirs)

indexHandler :: Handler IrcSnaplet IrcSnaplet ()
indexHandler = do
  s <- ask
  st <- liftIO $ readIORef (view (iconfig . cstate) s)
  
  renderWithSplices "index" (allSplices (_rcirs st))
