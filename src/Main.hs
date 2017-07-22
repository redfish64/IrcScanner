{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Snap
import IrcScanner.IrcSnaplet
import IrcScanner.Types
import Data.IORef(newIORef)
import IrcScanner.Index
import Data.Text as T
import Data.Time

main :: IO ()
main =
  do
    es <- _demoIState
    case es of
      Left x -> putStrLn("Error: " ++ (unpack x))
      Right s ->
        do
          i <- newIORef s
          (_, site, _) <- runSnaplet Nothing $ ircSnapletInit $ IConfig i (hoursToTimeZone 0) 
          quickHttpServe site

