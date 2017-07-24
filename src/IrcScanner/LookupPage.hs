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
    keyword <- getParamET "keyword" >>= (return . decodeUtf8)
    lift $ renderWithSplices "lookup" (lookupPageSplices keyword )
    return ()

    
lookupPageSplices :: Text -> Splices (SnapletISplice IrcSnaplet)
lookupPageSplices k = "keyword" ## textSplice k
