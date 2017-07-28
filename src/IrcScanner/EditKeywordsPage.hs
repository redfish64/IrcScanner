{-# LANGUAGE OverloadedStrings #-}
module IrcScanner.EditKeywordsPage where

import IrcScanner.Types as TY
import           Snap
import           Snap.Snaplet.Heist
import           Control.Lens
import Control.Monad.IO.Class(liftIO)
--import Control.Monad.Trans(lift)
import Control.Monad.Reader(ask,lift)
import Data.IORef(readIORef)
import Heist.Interpreted
import Heist
import Data.Map.Syntax((##))
import Data.Text as T(Text,lines,pack,append)
-- import Control.Monad.Trans.Either (runEitherT,left, EitherT(..))
-- import Control.Monad.Trans (lift)
-- import Data.ByteString(ByteString)
import Data.Text.Encoding(decodeUtf8,encodeUtf8)
import IrcScanner.SnapUtil
import IrcScanner.KeywordRulesParser
import IrcScanner.Index(tryIndex,addIndex,deleteAllIndexes)

allSplices :: Text -> Splices (SnapletISplice x)
allSplices rfc =
  do
    "rulesFileContents" ## textSplice rfc
    "results" ## textSplice ""


editKeywordsHandler :: HasHeist x => Handler x IrcSnaplet ()
editKeywordsHandler = do
  s <- ask
  st <- liftIO $ readIORef (view (iconfig . cstate) s)
  
  renderWithSplices "edit_keywords" (allSplices (_skwFileContents st))

editKeywordsTestHandler :: Handler IrcSnaplet IrcSnaplet ()
editKeywordsTestHandler = handleETHandler $ do
  rulesFileContents <- getParamET "rulesFileContents" >>= (return . decodeUtf8)

  results <- return $ parseKwFile $ T.lines rulesFileContents
  lift $ logError $ encodeUtf8 $ "result is " `append` (pack (show results))
  case results of
    Left errors -> lift $ renderWithSplices "edit_keywords_errors" $ kwErrorsSplices errors 
    Right indexes -> lift $ renderWithSplices "edit_keywords_indexes" $ kwIndexesSplices indexes

editKeywordsSaveHandler :: Handler IrcSnaplet IrcSnaplet ()
editKeywordsSaveHandler = handleETHandler $ do
  rulesFileContents <- getParamET "rulesFileContents" >>= (return . decodeUtf8)

  results <- return $ parseKwFile $ T.lines rulesFileContents
  lift $ logError $ encodeUtf8 $ "result is " `append` (pack (show results))
  case results of
    Left errors -> lift $ renderWithSplices "edit_keywords_errors" $ kwErrorsSplices errors 
    Right indexes -> lift $ do
      runIST' $
        do
          c <- ask
          lift $ saveKwFile rulesFileContents c
      runIST' $ deleteAllIndexes >> mapM addIndex indexes
      render "edit_keywords_save_results"

kwErrorsSplices :: [Text] -> Splices (SnapletISplice IrcSnaplet)
kwErrorsSplices lns =
  do
    "errors" ## mapSplices kwErrorSplice $ filter (\(l,_) -> l /= "") $
      Prelude.zip lns [1..] 


kwErrorSplice ::  Monad x => (Text, Int) -> Splice x
kwErrorSplice (ln, i) =
  do
    runChildrenWithText $
      do
        "errorText" ## ln
        "errorLine" ## (pack (show i))
    
kwIndexesSplices :: [TY.Index] -> Splices (SnapletISplice IrcSnaplet)
kwIndexesSplices indexes =
  do
    "indexes" ## mapSplices kwIndexSplice indexes
    

kwIndexSplice :: TY.Index -> SnapletISplice IrcSnaplet
kwIndexSplice ind =
  do
    ranges <- runIST $ tryIndex ind
    runChildrenWithText $
      do
        "displayName" ## (_idisplayName ind)
        "matchCount" ## (pack (show (length ranges)))
    
    

  
  
    
  
  -- filter (\l -> l /= "") $ mapInd doit lns
  -- where
  --   doit :: Text -> Int -> Text
  --   doit "" _ = ""
  --   doit t i = (show i) `append` ": " `append` t
    
    

