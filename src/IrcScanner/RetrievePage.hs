{-# LANGUAGE OverloadedStrings #-}
module IrcScanner.RetrievePage where

import IrcScanner.Types
import           Snap
import           Snap.Snaplet.Heist
import           Control.Lens
--import Control.Monad.Trans(lift)
--import Heist.Interpreted
import Heist
import Data.Map.Syntax((##))
import Data.Text(append)
import Control.Monad.Trans.Either (EitherT(..))
import Control.Monad.Trans (lift)
-- import Data.ByteString(ByteString)
import Data.Text.Encoding
import IrcScanner.SnapUtil
import IrcScanner.Util
import IrcScanner.Index
import Data.Text
import Heist.Interpreted
import qualified  Text.XmlHtml as X
--import Heist
import Data.Foldable (toList)

retrieveHandler :: EitherT Text (Handler IrcSnaplet IrcSnaplet) ()
retrieveHandler =
  do
    st <- lift $ getState
    kw <- getParamET "kw" >>= (return . decodeUtf8)
    
    cir <- EitherT $ return (justOrError ("Can't find index '" `append` kw) $ lookupCir st kw)
        
    lift $ renderWithSplices "retrieve" (allSplices cir)
    

cirSplice :: Monad x => CachedIndexResult -> Splice x
cirSplice cir =
  return $ toList $ fmap (\r -> X.Element "range" [("srow", row rstartPos r),
                                 ("scol", col rstartPos r),
                                 ("erow", row rendPos r),
                                 ("ecol", col rendPos r)] [])
         (_cranges cir)
  where
    row l r = pack $ show $ view (l . prow) r
    col l r = pack $ show $ view (l . pcol) r
    
  -- 
  -- [X
    
allSplices :: CachedIndexResult -> Splices (SnapletISplice IrcSnaplet)
allSplices cir = "Ranges" ## cirSplice cir
