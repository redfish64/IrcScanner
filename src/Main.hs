{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Snap
import IrcScanner.IrcSnaplet
import IrcScanner.Types
import Data.IORef(newIORef)
import IrcScanner.Index
import Data.Text as T
import Data.Time
import IrcScanner.LogWatcher
--import Control.Concurrent (threadDelay)

main :: IO ()
main =
  do
    es <- _loadIState "keywordRules.txt"
    case es of
      Left x -> putStrLn("Error: " ++ (unpack x))
      Right s ->
        do
          i <- newIORef s
          ic <- return $ IConfig i (hoursToTimeZone 0) "keywordRules.txt"
          watchLogFile "test.log" ic
          (_, site, _) <- runSnaplet Nothing $ ircSnapletInit ic
          quickHttpServe site

--          threadDelay (100 * 1000*1000)
