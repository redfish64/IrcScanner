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

main :: IO ()
main =
  do
    i <- newIORef emptyIState
    mvar <- newMVar ()

    ic <- return $ IConfig i (hoursToTimeZone 0) "keywordRules.txt" mvar

    es <- createInitialIState ic 
    case es of
      Left x -> putStrLn("Error: " ++ (unpack x))
      Right _ ->
        do
          watchLogFile "autonomic-dev.log" "autonomic-dev.log" ic
          watchLogFile "autonomic.log" "autonomic.log" ic
          (_, site, _) <- runSnaplet Nothing $ ircSnapletInit ic
          quickHttpServe site
  
--          threadDelay (100 * 1000*1000)
