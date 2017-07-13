module SnapUtil where

import Data.Text(append,Text)
import Data.ByteString(ByteString)
import Control.Monad.Trans.Either (EitherT(..))
import           Snap
--import           Snap.Snaplet.Heist
import Types
import Control.Monad.Trans (lift)
import Data.Text.Encoding

getParamET :: Text -> EitherT Text (Handler IrcSnaplet IrcSnaplet) ByteString
getParamET p = --undefined
  lift (getParam $ encodeUtf8 p) >>= doit
  where
    doit :: Maybe ByteString -> EitherT Text (Handler IrcSnaplet IrcSnaplet) ByteString
    doit x =
      EitherT $ return (maybe (Left $ "'" `append` p `append` "' not specified")
                        Right x)
