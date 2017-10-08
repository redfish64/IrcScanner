{-# OPTIONS -Wall #-}

module Learn.MonitorFile where

import Control.Concurrent (threadDelay)
import System.Environment (getArgs)
import System.INotify

main :: IO ()
main = do
        args <- getArgs
        if (null args)
                then usage
                else watch args

usage :: IO ()
usage = putStrLn "Usage: watch-read file ..."

watch :: [FilePath] -> IO ()
watch fs = withINotify $ \inotify -> do
        print fs
        mapM_ (\f -> addWatch inotify [Modify, CloseWrite] f (handleEvent f)) fs
        threadDelay (100 * microsecsPerSec)
    where
        handleEvent :: FilePath -> Event -> IO ()
        handleEvent f e = putStrLn (f ++ ": " ++ show e)

        microsecsPerSec = 1000000

        
