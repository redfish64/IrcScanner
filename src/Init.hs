module Init where

import Types(IState(..))
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

--we use a global variable to store state, so that the bot code can be built independently
--without holding state of the web code (this is for simplicity sake)
--TODO 3 ideally we would pass this to the bot code which would hold as part of its state
globalIndexCache :: IORef IState
{-# NOINLINE globalIndexCache #-}
globalIndexCache = unsafePerformIO (newIORef $ IState [])
  
