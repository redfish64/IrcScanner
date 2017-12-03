{-# LANGUAGE OverloadedStrings #-}
module Main where

import Snap
import IrcScanner.IrcSnaplet
import IrcScanner.Types
import Data.IORef(newIORef)
import IrcScanner.Index
import Data.Text as T
import Data.Time
import IrcScanner.LogWatcher
--import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar(newMVar)
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class
import Control.Monad.Reader
import Control.Lens
import Control.Concurrent

initState :: EitherT Text IO IConfig
initState =
  do
    i <- lift $ newIORef emptyIState
    mvar <- lift $ newMVar ()

    ic <- return $ IConfig i (hoursToTimeZone 0) "keywordRules.txt" mvar

    loadKwTemplateFile ic

    return ic

main :: IO ()
main =
  do
    eic <- runEitherT $ initState
    
    case eic of
      Left x -> putStrLn("Error: " ++ (unpack x))
      Right ic ->
        do
          --watchLogFile "nomiccoin-dev.log" "nomiccoin-dev.log" ic
          --threadDelay $ 1000*1000
          watchLogFile "nomiccoin.log" "nomiccoin.log" ic
          (_, site, _) <- runSnaplet Nothing $ ircSnapletInit ic
          quickHttpServe site
  
--          threadDelay (100 * 1000*1000)
