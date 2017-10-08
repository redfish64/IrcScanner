module Lens where

import Control.Lens
import Data.Map.Lens
import Data.Map

-- | gets "b" from this complicated structure. ^. is synonym for "get"
ex1 = ("hello",fromList [(1,"a"),(2,"b")]) ^. (_2 . (at 2) . _Just)

-- | like ex1, but returns nothing because "3" isn't in structure
ex2 = ("hello",fromList [(1,"a"),(2,"b")]) ^? (_2 . (at 3) . _Just)

-- | Here we set "b" to "c"
ex3 = (_2 . (at 2) . _Just) .~ "c" $ ("hello",fromList [(1,"a"),(2,"b")]) 

-- | Here we create a new entry, (3,"c")
ex4 = (_2 . (at 3)) .~ (Just "c") $ ("hello",fromList [(1,"a"),(2,"b")]) 
