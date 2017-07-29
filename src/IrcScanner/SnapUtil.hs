{-# LANGUAGE OverloadedStrings #-}
module IrcScanner.SnapUtil where

import Data.Text(append,Text)
import Data.ByteString(ByteString)
import Control.Monad.Trans.Either (EitherT(..))
import           Snap
import           Snap.Snaplet.Heist
import IrcScanner.Types
import Control.Monad.Trans (lift)
import Control.Monad.Reader(ask,runReaderT)
import Control.Monad.IO.Class(liftIO)
import Data.IORef(readIORef)
import           Control.Lens(view)
import Data.Text.Encoding(decodeUtf8, encodeUtf8)
import Heist(HeistT)



getParamET :: Text -> EitherT Text (Handler IrcSnaplet IrcSnaplet) ByteString
getParamET p = --undefined
  lift (getParam $ encodeUtf8 p) >>= doit
  where
    doit :: Maybe ByteString -> EitherT Text (Handler IrcSnaplet IrcSnaplet) ByteString
    doit x =
      EitherT $ return (maybe (Left $ "'" `append` p `append` "' not specified")
                        Right x)

getTextParamOrDefault :: HasHeist x => Text -> Text -> Handler x IrcSnaplet Text
getTextParamOrDefault p d = --undefined
  do
    maybeText <- (getParam $ encodeUtf8 p)
    return $ maybe d id (fmap decodeUtf8 maybeText)

getTextParam :: HasHeist x => Text -> Handler x IrcSnaplet (Maybe Text)
getTextParam p = --undefined
  do
    v <- (getParam $ encodeUtf8 p)
    return $ fmap decodeUtf8 v


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


--runIST :: SnapletISplice IrcSnaplet
runIST :: IST IO x -> HeistT
          (Handler IrcSnaplet IrcSnaplet)
          (Handler IrcSnaplet IrcSnaplet)
          x
runIST ist = 
  do
    s <- ask 
    lift $ liftIO $ runReaderT ist (_iconfig s)
    
runIST' :: IST IO x -> 
          (Handler IrcSnaplet IrcSnaplet)
          x
runIST' ist = 
  do
    s <- ask 
    liftIO $ runReaderT ist (_iconfig s)
    
