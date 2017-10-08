{-# LANGUAGE OverloadedStrings #-}
module IrcScanner.Index(tryIndex,addIndex,addIndexes,getIndexes,deleteIndex,addFileLine, addFileLines, getIState, updateIState,lookupCir,addIndex',createInitialIState,deleteAllIndexes,getIrssiMessageText,addLogFileToState) where

import Control.Lens.At (at)
import Control.Lens.Setter ((.~))
import IrcScanner.Types
import Data.Sequence as S
import Control.Monad.Trans.Reader as R
import Data.Text.ICU as I
import Data.Text as T
import Control.Monad.State as S
import IrcScanner.Util
import Test.Hspec
import Data.Maybe (fromMaybe)
import Data.Foldable (toList)
import Control.Monad.IO.Class(liftIO)
import Data.IORef
import qualified Control.Lens as L
import Control.Monad.Trans.Either
--import Control.Exception(try, SomeException)
--import qualified Data.Text.IO as DTI
import Parser.Irssi.Log.Types(LogType(..),MessageContent)
import Parser.Irssi.Log.Util.Import(importIrssiData)
import Data.Time.LocalTime(localTimeToUTC)
import qualified Data.List as LI(find)
import Data.Text.IO as I(readFile)
import IrcScanner.KeywordRulesParser
import Prelude as P

--tries running an index against the log and returns a result (without saving it)
tryIndex :: Index -> IST IO [Range]
tryIndex i =
  do
    s <- getIState
    let cir =  buildCachedIndexResult s i
      in return (toList (L.view cranges cir))

--adds and saves an index to state
addIndex :: Index -> IST IO ()
addIndex x = addIndexes [x]


-- | concats all file lines together in one seq
getAllFileLines :: IState -> Seq ILine
getAllFileLines is =
  let
    files = (L.view sfiles is)
    in P.foldr (S.><) (fromList [])
       (S.fmap (L.view flines) files)

--builds a complete cached index result from scratch
buildCachedIndexResult :: IState -> Index -> CachedIndexResult
buildCachedIndexResult s i =
  let
    joinedLines = getAllFileLines s
  in updateCachedIndexResult joinedLines (emptyCacheIndexResult i)


-- returns only text from essages and actions
getIrssiMessageText :: LogType -> MessageContent
getIrssiMessageText (Message _ _ _ mc) = mc
getIrssiMessageText (Action _ _ mc) = mc
getIrssiMessageText _ = ""


--adds and saves an index to state given text of a matcher
--if matcher can't be understood, returns left
addIndex' :: Text -> MatcherType -> Text -> EIST IO Index
addIndex' n t r =
  do
    matcher <- EitherT (lift $ return (mkMatcher t r))
    let i = Index n matcher
      in
      do
        lift $ addIndex i
        return i

_addFile :: Text -- ^ file "nick name" (what file is refered to internally
  -> Text -- ^ filename
  -> EIST IO ()
_addFile k f = 
  do
    c <- lift $ ask
    d <- liftIO $ importIrssiData (unpack f)
    lift $ addFileLines k $ fmap (\(t,l) -> ILine (localTimeToUTC (_ctimeZone c) t) l) d


--convertMonadEither :: Monad m => m (Either a x) -> m (Either 
-- toEIST = undefined

--adds multiple indexes to state
addIndexes :: [Index] -> IST IO ()
addIndexes is = updateIState
  (\s -> (s { _scirs =
              fmap (buildCachedIndexResult s) is
              ++ (_scirs s) },()))
  

getIState :: IST IO IState
getIState = (R.ask >>= (liftIO . readIORef . _cstate))

updateIState :: (IState -> (IState,v)) -> IST IO v
updateIState f =
  do
    c <- R.ask
    liftIO (atomicModifyIORef' (_cstate c) f)
    
--  R.ask >>= liftIO . 
      

--returns list of indexes in memory
getIndexes :: IST IO [Index]
getIndexes =
  do
    s <- getIState
    return $ fmap _cindex (_scirs s)

deleteAllIndexes :: IST IO ()
deleteAllIndexes = updateIState (\s -> (s { _scirs = [] },()))
       

--deletes index from memory and cached results
--returns true if index is found, false otherwise
--arg is the displayname
deleteIndex :: Text -> IST IO Bool
deleteIndex name =
  updateIState
  (\s ->
     let
       s' = s { _scirs = P.filter (\cir -> (_idisplayName . _cindex) cir == name) (_scirs s) }
       in
       (s',P.length (_scirs s) == P.length (_scirs s'))
  )
  


addLogFileToState :: Text -> IST IO ()
addLogFileToState fnn =
  updateIState
    (\s -> ( ((sfiles . (at fnn)) .~ (Just emptyFile) $ s), () ))

addFileLine :: Text -> ILine -> IST IO ()
addFileLine n l = addFileLines n [l]

-- | adds file lines to cache of a particular irc file (should be called by the log watcher)
addFileLines :: Text -- ^ the nick name of the file, ex "#autonomic" for "../irclogs/#autonomic.log"
  -> [ILine] -- ^ lines to add
  -> IST IO ()
addFileLines k ls =
  -- add the given lines to the specific file in the IState structure, looked up by "k" with sfiles
  updateIState
    (\s -> (refreshIndexCache (L.over (sfiles . (at k) . L._Just . flines) (\cls -> cls >< (fromList ls)) s), ()))


refreshIndexCache :: IState -> IState
refreshIndexCache is =
         let lns = getAllFileLines is
         in
           L.over scirs (fmap (updateCachedIndexResult lns)) is

updateCachedIndexResult :: Seq ILine -> CachedIndexResult -> CachedIndexResult
updateCachedIndexResult f cir
      | (_cendLine cir) >= (S.length f) = cir
      | otherwise =
        let
          line = (_cendLine cir)
          lineText = getIrssiMessageText (_llogType (f `S.index` line))
          matches = runMatcher (_imatcher . _cindex $ cir) lineText
          cir' = cir { _cranges =
                      (_cranges cir) >< (fromList (fmap (\(s,e) -> (Range (Pos line s) (Pos line e))) matches)) }
        in
          updateCachedIndexResult f (cir' { _cendLine = (_cendLine cir') + 1 })
          
                                    
    
runMatcher :: Matcher -> Text -> [(Int,Int)]
runMatcher mr l =
  let matches = findAll mr l
  in
    evalState (mapM getMatchRange matches) 0
  where
    getMatchRange :: Match -> State Int (Int,Int)
    getMatchRange m =
      do
        lastPos <- S.get
        (let 
          ms = (T.length (I.span m)) + lastPos
          me = ms + (T.length (fromMaybe undefined (I.group 0 m)))
         in
           do
             S.put me
             return (ms,me)
          )


--creates a fake demo IState for testing
-- _demoIState :: IO (Either Text IState)
-- _demoIState =
--   do
--     i <- newIORef $ emptyIState
--     runReaderT (runEitherT createDemo) (IConfig i (hoursToTimeZone 0))
--   where
--     createDemo :: EIST IO IState 
--     createDemo = 
--       do
--         addIndex' "AutoNomic" RegexMatcher "/\\bautonomic\\b/i"
--         addIndex' "Cool" RegexMatcher "/cool/i"
--         addIndex' "Open/Closed Phase" RegexMatcher "/"
        
--         c <- lift ask
--         s <- liftIO $ readIORef (_cstate c)
--         return s

createInitialIState :: IConfig -> IO (Either Text IState)
createInitialIState ic = --undefined
  do
    kwFileContents <- I.readFile (_crulesFile ic)
    runReaderT (runEitherT $ doit kwFileContents) ic
  where
    doit :: Text -> EIST IO IState
    doit kwFileContents = 
      do
        --load indexes from keyword file
        indexes <- EitherT $ return $ replaceLeft transformKwError $ parseKwFile $ T.lines kwFileContents
        --insert them into the state
        _ <- lift $ mapM addIndex indexes

        lift $ updateIState (\s -> (s { _skwFileContents = kwFileContents },()))
        
        --get and return the state
        lift $ getIState
    transformKwError :: [Text] -> Text
    transformKwError t = T.unlines $ fmap
      (\l -> "Error parsing " `append` (pack $ _crulesFile ic) `append` ": " `append` l)  t

_test :: IO ()
_test =
  hspec $ do
  describe "runMatcher" $ do
    it "matches 3 foos" $ do
      runMatcher (either undefined id $ parseRegexWithFlags "/foo/") "abc foo def foo ghi foo"
        `shouldBe` [(4,7),(12,15),(20,23)]
    it "matches no foos" $ do
      runMatcher (either undefined id $ parseRegexWithFlags "/foo/") "abc def ghi"
        `shouldBe` []



lookupCir :: IState -> Text -> Maybe CachedIndexResult
lookupCir s n = 
  LI.find ((== n) . (L.view (cindex . idisplayName))) (_scirs s)
