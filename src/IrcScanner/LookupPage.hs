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
import Text.XmlHtml(elementChildren)
--import Heist


lookupHandler :: Handler IrcSnaplet IrcSnaplet ()
lookupHandler =
  handleETHandler $ do
    keyword <- getParamET "keyword" >>= (return . decodeUtf8)
    lift $ renderWithSplices "lookup" (lookupPageSplices keyword )
    return ()

    
lookupPageSplices :: Text -> Splices (SnapletISplice IrcSnaplet)
lookupPageSplices k =
  do
    "keyword" ## textSplice k
    "logFollowMode" ## (return [])
    "keywordMode" ## (fmap elementChildren getParamNode)

followLogHandler :: Handler IrcSnaplet IrcSnaplet ()
followLogHandler =
  handleETHandler $ do
    fnn <- lift $ getTextParamOrDefault "fnn" "autonomic.log" -- default to autonomic.log
    lift $ renderWithSplices "lookup" (followLogSplices fnn )
    return ()

    
followLogSplices :: Text -> Splices (SnapletISplice IrcSnaplet)
followLogSplices fnn =
  do
    "fileNickName" ## textSplice fnn
    "keywordMode" ## (return [])
    "logFollowMode" ## (fmap elementChildren getParamNode)

    
