{-# LANGUAGE OverloadedStrings #-}
module Index(tryIndex,addIndex,addIndexes,getIndexes,deleteIndex,addFileLine, addFileLines, getIState, updateIState, _demoIState,lookupCir) where

import Types
import Data.Sequence as S
import Control.Monad.Trans.Reader as R
import Data.Text.ICU as I
import Data.Text as T
import Control.Monad.State as S
import Util
import Test.Hspec
import Data.Maybe (fromMaybe)
import Data.Foldable (toList)
import Control.Monad.IO.Class(liftIO)
import Data.IORef
import qualified Control.Lens as L
import Control.Monad.Trans.Either
import Control.Exception(try, SomeException)
import qualified Data.Text.IO as DTI
--import Data.List.Split

--tries running an index against the log and returns a result (without saving it)
tryIndex :: Index -> IST IO [Range]
tryIndex i =
  do
    s <- getIState
    let
      f = (L.view rfile s)
      cir = updateCachedIndexResult f (emptyCacheIndexResult i)
      in return (toList (L.view cranges cir))

--adds and saves an index to state
addIndex :: Index -> IST IO ()
addIndex x = addIndexes [x]


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

addFile :: Text -> EIST IO ()
addFile f =
  do
    
    text <-
      (EitherT $ liftIO $
       (do
           x <- (try $ DTI.readFile (unpack f) :: IO (Either SomeException Text))
           return $ (replaceLeft (pack . show) x)
       )
      )
    (lift $ addFileLines  $ splitOn "\n" text) 

--convertMonadEither :: Monad m => m (Either a x) -> m (Either 
-- toEIST = undefined

--adds multiple indexes to state
addIndexes :: [Index] -> IST IO ()
addIndexes is = updateIState
  (\s -> (s { _rcirs = fmap (\i -> (updateCachedIndexResult (_rfile s) (emptyCacheIndexResult i))) is
                     ++ (_rcirs s) },()))
  

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
    return $ fmap _cindex (_rcirs s)


--deletes index from memory and cached results
--returns true if index is found, false otherwise
--arg is the displayname
deleteIndex :: Text -> IST IO Bool
deleteIndex name =
  updateIState
  (\s ->
     let
       s' = s { _rcirs = Prelude.filter (\cir -> (_idisplayName . _cindex) cir == name) (_rcirs s) }
       in
       (s',Prelude.length (_rcirs s) == Prelude.length (_rcirs s'))
  )
  

addFileLine :: Text -> IST IO ()
addFileLine l = addFileLines [l]

addFileLines :: [Text] -> IST IO ()
addFileLines ls = updateIState
  (\s -> (refreshIndexCache (s { _rfile = (_rfile s) >< (fromList ls) }),()))

refreshIndexCache :: IState -> IState
refreshIndexCache is =
  is { _rcirs = fmap (updateCachedIndexResult (_rfile is)) (_rcirs is) }


updateCachedIndexResult :: Seq Text -> CachedIndexResult -> CachedIndexResult
updateCachedIndexResult f cir
      | (_cendLine cir) >= (S.length f) = cir
      | otherwise =
        let
          line = (_cendLine cir)
          matches = (runMatcher (_imatcher . _cindex $ cir) (f `S.index` line))
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
_demoIState :: IO (Either Text IState)
_demoIState =
  do
    i <- newIORef $ emptyIState
    runReaderT (runEitherT createDemo) (IConfig i)
  where
    createDemo :: EIST IO IState 
    createDemo = 
      do
        addFile "test.log"
        addIndex' "AutoNomic" RegexMatcher "/\b(autonomic|an)\b/"
        
        c <- lift ask
        s <- liftIO $ readIORef (_cstate c)
        return s
        


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
lookupCir = undefined
