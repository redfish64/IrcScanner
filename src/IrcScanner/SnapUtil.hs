{-# LANGUAGE OverloadedStrings #-}
module IrcScanner.SnapUtil where

import Data.Text(append,Text)
import Data.ByteString(ByteString)
import Control.Monad.Trans.Either (EitherT(..))
import           Snap
--import           Snap.Snaplet.Heist
import IrcScanner.Types
import Control.Monad.Trans (lift)
import Data.Text.Encoding
import Control.Monad.Reader(ask)
import Control.Monad.IO.Class(liftIO)
import Data.IORef(readIORef)
import           Control.Lens(view)


getParamET :: Text -> EitherT Text (Handler IrcSnaplet IrcSnaplet) ByteString
getParamET p = --undefined
  lift (getParam $ encodeUtf8 p) >>= doit
  where
    doit :: Maybe ByteString -> EitherT Text (Handler IrcSnaplet IrcSnaplet) ByteString
    doit x =
      EitherT $ return (maybe (Left $ "'" `append` p `append` "' not specified")
                        Right x)

handleETHandler :: EitherT Text (Handler IrcSnaplet IrcSnaplet) () -> Handler IrcSnaplet IrcSnaplet ()
handleETHandler et = do
  lr <- runEitherT et
  case lr of
    Left x ->
      logError $ encodeUtf8 x
    Right _ -> return ()


getState :: Handler IrcSnaplet IrcSnaplet IState
getState = do
  s <- (ask :: Handler IrcSnaplet IrcSnaplet IrcSnaplet)
  liftIO $ readIORef (view (iconfig . cstate) s)


