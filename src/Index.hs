module Index(tryIndex,addIndex,addIndexes,getIndexes,deleteIndex) where

import Types

--tries running an index against the log and returns a result (without saving it)
tryIndex :: Index -> IO IndexResult
tryIndex = undefined

--adds and saves an index to state
addIndex :: Index -> IO IndexResult
addIndex x = addIndexes [x] >>= return . head

--adds multiple indexes to state
addIndexes :: [Index] -> IO [IndexResult]
addIndexes = undefined

--returns list of indexes in memory
getIndexes :: IO [Index]
getIndexes = undefined


--deletes index from memory and cached results
--returns true if index is found, false otherwise
deleteIndex :: Index -> IO Bool
deleteIndex = undefined

