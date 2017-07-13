module LookupPage where

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
import Control.Monad.Trans.Either (runEitherT,left, EitherT(..))
import Control.Monad.Trans (lift)
-- import Data.ByteString(ByteString)
import Data.Text.Encoding
import SnapUtil
import Util
import Index

lookupHandler :: Handler IrcSnaplet IrcSnaplet ()
lookupHandler =
  do
    s <- ask
    st <- liftIO $ readIORef (view (iconfig . cstate) s)
  
    lr <- runEitherT
      (do
        name <- getParamET "name" >>= (return . decodeUtf8)
        cir <- EitherT $ return (justOrError ("Can't find index '" `append` name) $ lookupCir st name)
        
        lift $ renderWithSplices "lookup" (allSplices cir)
      )

    case lr of
      Left x ->
        logError $ encodeUtf8 x
      Right y -> return ()

lookupSplices :: 
