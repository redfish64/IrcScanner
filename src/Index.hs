module Index(tryIndex,addIndex,addIndexes,getIndexes,deleteIndex,addFileLine, addFileLines) where

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

--tries running an index against the log and returns a result (without saving it)
tryIndex :: Index -> IST IO [Range]
tryIndex i =
  do
    s <- getIState
    let
      f = (rfile s)
      cir = updateCachedIndexResult f (emptyCacheIndexResult i)
      in return (toList (cranges cir))

--adds and saves an index to state
addIndex :: Index -> IST IO ()
addIndex x = addIndexes [x]

--adds multiple indexes to state
addIndexes :: [Index] -> IST IO ()
addIndexes is = updateIState
  (\s -> (s { rcirs = fmap (\i -> (updateCachedIndexResult (rfile s) (emptyCacheIndexResult i))) is
                     ++ (rcirs s) },()))
  

getIState :: IST IO IState
getIState = (R.ask >>= (liftIO . readIORef . cstate))

updateIState :: (IState -> (IState,v)) -> IST IO v
updateIState f =
  do
    c <- R.ask
    liftIO (atomicModifyIORef' (cstate c) f)
    
--  R.ask >>= liftIO . 
      

--returns list of indexes in memory
getIndexes :: IST IO [Index]
getIndexes =
  do
    s <- getIState
    return $ fmap cindex (rcirs s)


--deletes index from memory and cached results
--returns true if index is found, false otherwise
--arg is the displayname
deleteIndex :: Text -> IST IO Bool
deleteIndex name =
  updateIState
  (\s ->
     let
       s' = s { rcirs = Prelude.filter (\cir -> (idisplayName . cindex) cir == name) (rcirs s) }
       in
       (s',Prelude.length (rcirs s) == Prelude.length (rcirs s'))
  )
  

addFileLine :: Text -> IST IO ()
addFileLine l = addFileLines [l]

addFileLines :: [Text] -> IST IO ()
addFileLines ls = updateIState
  (\s -> (refreshIndexCache (s { rfile = (rfile s) >< (fromList ls) }),()))

refreshIndexCache :: IState -> IState
refreshIndexCache is =
  is { rcirs = fmap (updateCachedIndexResult (rfile is)) (rcirs is) }


updateCachedIndexResult :: Seq Text -> CachedIndexResult -> CachedIndexResult
updateCachedIndexResult f cir
      | (cendLine cir) >= (S.length f) = cir
      | otherwise =
        let
          line = (cendLine cir)
          cir' =
            let matches = (runMatcher (imatcher . cindex $ cir) (f `S.index` line))
            in
              cir { cranges =
                      (cranges cir) >< (fromList (fmap (\(s,e) -> (Range (Pos line s) (Pos line e))) matches)) }
        in
          cir' { cendLine = (cendLine cir') + 1 }
          
                                    
    
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
