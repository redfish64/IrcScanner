{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module IrcScanner.IrcSnaplet where

import IrcScanner.Types
import           Snap
import           Snap.Snaplet.Heist
import IrcScanner.KeywordIndexPage
import IrcScanner.LookupPage
import IrcScanner.RetrievePage
import IrcScanner.LoadRowsPage
import IrcScanner.EditKeywordsPage
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
    ("followLog", followLogHandler),
    ("loadRows", handleETHandler $ loadRowsHandler),
    ("keyword_index", keywordIndexHandler),
    ("edit_keywords", editKeywordsHandler),
    ("edit_keywords_test", editKeywordsTestHandler),
    ("edit_keywords_save", editKeywordsSaveHandler),
    ("", render "index")
    ]
  return $ IrcSnaplet h c

  



-- getParam "name" >>=
--                  return .(maybe ")
--                           id))


      
  


