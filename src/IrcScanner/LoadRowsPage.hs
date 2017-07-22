{-# LANGUAGE OverloadedStrings #-}
module IrcScanner.LoadRowsPage where

import IrcScanner.Types
import           Snap
import           Snap.Snaplet.Heist
import           Control.Lens
--import Control.Monad.Trans(lift)
--import Heist.Interpreted
--import Heist
import Data.Map.Syntax((##))
import Control.Monad.Trans.Either (EitherT(..))
import Control.Monad.Trans (lift)
-- import Data.ByteString(ByteString)
import Data.Text.Encoding
import IrcScanner.SnapUtil
import IrcScanner.Util
import Data.Text as T
--import Heist.Interpreted
import qualified  Text.XmlHtml as X
--import Heist
import Data.Foldable (toList)
import Data.Sequence as S
--import Data.Time.Clock
--import Control.Lens.TH


loadRowsHandler :: EitherT Text (Handler IrcSnaplet IrcSnaplet) ()
loadRowsHandler =
  do
    st <- lift $ getState
    srow <- getParamET "srow" >>= (return . decodeUtf8) >>=
      readOrLeft "Can't parse srow"
    cnt <- getParamET "count" >>= (return . decodeUtf8) >>= readOrLeft "Can't parse count"

    if (cnt <= 0) then (EitherT $ return (Left "Count must be positive")) else return ()

    let lns = sliceSeq  srow cnt (view (sfile . flines) st)
        date = (_ltime (S.index lns 0))
      in
      lift $ renderWithSplices "loadRows" $
        do
          "Rows" ## (return (mapInd (lineToRow srow) (toList lns)))
          "Date" ## (return [X.Element "Date" [] [X.TextNode (pack (show date))]])

  where
    lineToRow :: Int -> ILine -> Int -> X.Node
    lineToRow indexOffset l i = 
        X.Element "Row" [("id",pack $ show (i + indexOffset)),("text",pack $ show (_llogType l))] []

-- cirSplice :: Monad x => CachedIndexResult -> Splice x
-- cirSplice cir =
--   return $ toList $ fmap (\r -> X.Element "range" [("srow", row rstartPos r),
--                                  ("scol", col rstartPos r),
--                                  ("erow", row rendPos r),
--                                  ("ecol", col rendPos r)] [])
--          (_cranges cir)
--   where
--     row l r = pack $ show $ view (l . prow) r
--     col l r = pack $ show $ view (l . pcol) r
    
  -- 
  -- [X
    
