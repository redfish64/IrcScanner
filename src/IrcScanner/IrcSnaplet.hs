{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module IrcSnaplet where

import Types
import           Snap
import           Snap.Snaplet.Heist
import           Control.Lens
import Control.Monad.IO.Class(liftIO)
--import Control.Monad.Trans(lift)
import Control.Monad.Reader(ask)
import Data.IORef(readIORef)
import Heist.Interpreted
import Heist
import Data.Map.Syntax((##))
import Data.Text(append,Text)
import Control.Monad.Trans.Either (runEitherT,left, EitherT(..))
import Control.Monad.Trans (lift)
import Data.ByteString(ByteString)
import Data.Text.Encoding
import IndexPage
import LookupPage

ircSnapletInit :: IConfig -> SnapletInit IrcSnaplet IrcSnaplet
ircSnapletInit c = makeSnaplet "irc" "Irc snaplet" Nothing $ do
  h <- nestSnaplet "heist" iheist $ heistInit "templates"
  --modifyHeistState $ bindAttributeSplices [("main-textbox", mainTextboxAttributeSplice)]
  addRoutes [
    ("", indexHandler),
    ("lookup", lookupHandler)
            ]
  return $ IrcSnaplet { _iheist = h,  _iconfig = c }

  



-- getParam "name" >>=
--                  return .(maybe ")
--                           id))


      
  


