module Bot where

import Types
import Index
import Control.Monad.IO.Class(liftIO)
import Data.Text(Text)
import Control.Monad.Reader
import Data.IORef(newIORef)

--this is the main method for the bot
botMain :: IST IO ()
botMain =
  --TODO example code
  do
    addFileLine "abc foo def"
    addFileLine "ghi"
    addFileLine "foo jkl"
    withIndex "foo" ExactMatcher "foo"
      (\i ->
         do 
           i1r <- tryIndex i
           liftIO $ putStrLn $ "foo matches: " ++ show i1r)
        
    withIndex "foo" RegexMatcher "/\\b\\w\\w\\w\\b/" addIndex

    -- s <- getIState
    -- liftIO . putStrLn $ "s is " ++ show s
    return ()

--builds an index. If there is an error, prints out an error message, otherwise runs given
--monad
withIndex :: Text -> MatcherType -> Text -> (Index -> IST IO ()) -> IST IO ()
withIndex n t r m =
  let ematcher = (mkMatcher t r)
  in
    case ematcher of
      Left msg -> liftIO . putStrLn $ "Error: "++ (show msg)
      Right matcher -> m (Index n matcher)
    
    


_testBotMain :: IO ()
_testBotMain =
  do
    i <- newIORef $ emptyIState
    runReaderT botMain (IConfig i)
