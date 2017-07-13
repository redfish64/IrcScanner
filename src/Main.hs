{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Snap
import   IrcSnaplet
import Types
import Data.IORef(newIORef)
import Index
import Data.Text as T

main :: IO ()
main =
  do
    es <- _demoIState
    case es of
      Left x -> putStrLn("Error: " ++ (unpack x))
      Right s ->
        do
          i <- newIORef s
          (_, site, _) <- runSnaplet Nothing $ ircSnapletInit $ IConfig i
          quickHttpServe site

