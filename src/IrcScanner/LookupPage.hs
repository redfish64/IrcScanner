{-# LANGUAGE OverloadedStrings #-}
module IrcScanner.LookupPage where

import IrcScanner.Types
import           Snap
import           Snap.Snaplet.Heist
--import Control.Monad.Trans(lift)
--import Heist.Interpreted
import Heist
import Data.Map.Syntax((##))
import Control.Monad.Trans (lift)
-- import Data.ByteString(ByteString)
import Data.Text.Encoding
import IrcScanner.SnapUtil
import Data.Text
import Heist.Interpreted
--import Heist


lookupHandler :: Handler IrcSnaplet IrcSnaplet ()
lookupHandler =
  handleETHandler $ do
    name <- getParamET "name" >>= (return . decodeUtf8)
    lift $ renderWithSplices "lookup" (allSplices name )
    return ()

    
allSplices :: Text -> Splices (SnapletISplice IrcSnaplet)
allSplices n = "keywordName" ## textSplice n
