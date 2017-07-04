module Index(tryIndex,addIndex,addIndexes,getIndexes,deleteIndex,addFileLine, addFileLines) where

import Types
import Data.Sequence
import Control.Monad.Trans.RWS
import qualified Data.List as L

--tries running an index against the log and returns a result (without saving it)
tryIndex :: Index -> IST IO IndexResult
tryIndex = undefined

--adds and saves an index to state
addIndex :: Index -> IST IO ()
addIndex x = addIndexes [x]

--adds multiple indexes to state
addIndexes :: [Index] -> IST IO ()
addIndexes = undefined
      

--returns list of indexes in memory
getIndexes :: IST IO [Index]
getIndexes = undefined


--deletes index from memory and cached results
--returns true if index is found, false otherwise
deleteIndex :: Index -> IST IO Bool
deleteIndex = undefined

addFileLine :: String -> IST IO ()
addFileLine l = addFileLines [l]

addFileLines :: [String] -> IST IO ()
addFileLines ls = modify (\is -> refreshIndexCache
                                 (is { rfile = (rfile is) >< (fromList ls) }))

                  

refreshIndexCache :: IState -> IState
refreshIndexCache is =
  is { rcirs = fmap (updateCachedIndexResult (rfile is)) (rcirs is) }


updateCachedIndexResult :: Seq String -> CachedIndexResult -> CachedIndexResult
updateCachedIndexResult f cir
      | (cendLine cir) >= (Data.Sequence.length f) = cir
      | otherwise =
        let
          line = (cendLine cir)
          cir' = case (runMatcher (imatcher . cindex $ cir) (f `index` line)) of
                     Nothing -> cir
                     Just (scol,ecol) ->
                       cir { cranges =
                               Range (Pos line scol) (Pos line ecol) : (cranges cir) }
        in
          cir' { cendLine = (cendLine cir') + 1 }
          
                                    
    
runMatcher :: Matcher -> String -> Maybe (Int,Int)
runMatcher m l =
  let v = (mval m)
  in
    case (mtype m) of
      ExactMatcher -> Just (L.isInfixOf v l, (L.length v))
