module IrcScanner.LogWatcher(watchLogFile) where

import IrcScanner.Types
import Data.IORef
import System.INotify
import Control.Monad.Trans.Reader as R
import Control.Monad.IO.Class(liftIO)
import           System.IO as I
import Data.Time.LocalTime(localTimeToUTC)--, hoursToTimeZone)
import Parser.Irssi.Log.Util.Import(importIrssiDataContents)
import IrcScanner.Index(addFileLines)
import Data.Text as T (length)--,unpack) 
import Data.Text.IO as T (hGetContents)

--import IrcScanner.Index
--import Control.Concurrent (threadDelay)


data WatchData =
  WatchData {
  fileName :: FilePath,
  currPos :: Integer
  } deriving (Show)

--watches the log file for updates, and adds rows to state when new lines appear
watchLogFile :: FilePath -> IConfig ->  IO ()
watchLogFile fn ic = do
  inotify <- initINotify
  wd <- liftIO $ newIORef (WatchData fn 0)
  liftIO $ addWatch inotify [Modify, CloseWrite] fn (handleEvent wd ic)
  handleEvent wd ic Ignored
  return ()
   
handleEvent :: IORef WatchData ->  IConfig -> Event -> IO ()
handleEvent wdir ic _ =
  do
    putStrLn("handleEvent start");
    --this assumes that the haskell inotify code doesn't use multiple threads for
    --events that occur one after another
    --open the file and read the data
    wd <- readIORef wdir
    h <- openFile (fileName wd) ReadMode
    hSeek h AbsoluteSeek (currPos wd)
    hSetEncoding h latin1
    contents <- T.hGetContents h
    irssiData <- importIrssiDataContents contents
    if (T.length contents > 0) then putStrLn("currPos "++(show $ currPos wd)++" read "++(show (T.length contents))++" bytes") else return ()
    if (T.length contents < 10000) then putStrLn("contents "++(show $ contents)) else return ()

    --update our internal state for the new current pos
    writeIORef wdir $ wd { currPos = (currPos wd) + (fromIntegral (T.length contents)) }

    --notify the main code that there are new lines
    runReaderT
      (addFileLines $ fmap (\(t,l) -> ILine (localTimeToUTC (_ctimeZone ic) t) l) irssiData)
      ic

    putStrLn("handleEvent end");


-- _watchLogTest :: IO ()          
-- _watchLogTest =
--   do
--     es <- _demoIState 
--     case es of
--       Left x -> putStrLn("Error: " ++ (unpack x))
--       Right s ->
--         do
--           i <- newIORef s
--           ic <- return $ IConfig i (hoursToTimeZone 0)
--           watchLogFile "t2.log" ic
--           loopAndPrintStatus ic
--   where
--     loopAndPrintStatus :: IConfig -> IO ()
--     loopAndPrintStatus ic = do
--       s <- readIORef (_cstate ic)
--       putStrLn $ show (_sfile s)
--       threadDelay (10 * 1000 * 1000)
--       loopAndPrintStatus ic
      
      
