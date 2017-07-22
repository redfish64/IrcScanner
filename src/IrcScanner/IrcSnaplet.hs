{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module IrcScanner.IrcSnaplet where

import IrcScanner.Types
import           Snap
import           Snap.Snaplet.Heist
import IrcScanner.IndexPage
import IrcScanner.LookupPage
import IrcScanner.RetrievePage
import IrcScanner.LoadRowsPage
import Data.Text(pack)
--import           Control.Lens
import Snap.Util.FileServe
import IrcScanner.SnapUtil

ircSnapletInit :: IConfig -> SnapletInit IrcSnaplet IrcSnaplet
ircSnapletInit c = makeSnaplet "irc" "Irc Scanner thingy" Nothing $ do
  h <- nestSnaplet "heist" iheist $ heistInit "templates"
  --modifyHeistState $ bindAttributeSplices [("main-textbox", mainTextboxAttributeSplice)]
  getEnvironment >>= printInfo . pack . ("IrcSnaplet: " ++)
  addRoutes [
    ("files",  serveDirectory "static"),
    ("retrieve", handleETHandler $ retrieveHandler),
    ("lookup", lookupHandler),
    ("loadRows", handleETHandler $ loadRowsHandler),
    ("", indexHandler)
            ]
  return $ IrcSnaplet h c 0

  



-- getParam "name" >>=
--                  return .(maybe ")
--                           id))


      
  


