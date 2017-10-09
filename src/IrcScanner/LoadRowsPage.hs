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
import Control.Monad.Trans.Either (EitherT(..),hoistEither)
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
import Parser.Irssi.Log.Types
import qualified Numeric.Search as SE
import Test.Hspec
--import IrcScanner.Index(getIrssiMessageText)


{- | loads rows from the file
     takes params:
     filenick -- nick name of file
     srow -- where to start from. If srow is negative, then the starting place is put
        at the end of the file minus count
    count -- number of rows to load
    keyword -- optional parameter. If present instances of this keyword will be highlighted in log
 -}
loadRowsHandler :: EitherT Text (Handler IrcSnaplet IrcSnaplet) ()
loadRowsHandler =
  do
    st <- lift $ getState
    filenick <- getParamET "fnn" >>= (return . decodeUtf8)
    srow <- getParamET "srow" >>= (return . decodeUtf8) >>=
      readOrLeft "Can't parse srow"
    cnt <- getParamET "count" >>= (return . decodeUtf8) >>= readOrLeft "Can't parse count"
    mkeyword <- lift $ getTextParam "keyword"

    if (cnt <= 0) then (EitherT $ return (Left "Count must be positive")) else return ()

    allLns <- hoistEither $ justOrError ("Can't find file for nick " `T.append` filenick) $ st ^? (sfiles . (at filenick) . _Just . flines) 
    mranges <- return $ fmap (getRangesForKeyword st) mkeyword

    -- lift $ logError $ encodeUtf8 $ T.pack $  "myranges is "++
    --   (show $ fmap (\l -> rangesForLn l (maybe S.empty id mranges)) [srow..(srow+cnt-1)])

    -- lift $ logError $ encodeUtf8 $ T.pack $  "psMessages is "++
    --   (show $ fmap (\l ->
    --                   let myRanges = rangesForLn l (maybe S.empty id mranges)
    --                       ln = S.index allLns l
    --                       txt = getIrssiMessageText (_llogType ln)
    --                       res = psMessage myRanges txt
    --                  in res)
    --     [srow..(srow+cnt-1)])

    -- lift $ logError $ encodeUtf8 $ T.pack $  "prettyShowLT is "++
    --   (show $ fmap (\l ->
    --                   let ln = S.index allLns l
    --                       res = prettyShowLT (_llogType ln) l (maybe S.empty id mranges)
    --                  in res)
    --     [srow..(srow+cnt-1)])

    -- lift $ logError $ encodeUtf8 $ T.pack $  "lineToRow is "++
    --   (show $ fmap (\l ->
    --                   let ln = S.index allLns l
    --                       res = lineToRow l (maybe S.empty id mranges) ln 0
    --                  in res)
    --     [srow..(srow+cnt-1)])

    -- lift $ logError $ encodeUtf8 $ T.pack $  "lineToRow2 is "++
    --   (show $ 
    --     let
    --       srow' = if srow < 0 then (Prelude.length allLns) - cnt else srow
    --       lns = sliceSeq srow' cnt (view (sfile . flines) st)
    --       res = mapInd (lineToRow srow' $ maybe S.empty id mranges) $ toList lns
    --     in res)
        

    -- lift $ logError $ encodeUtf8 $ T.pack $  "psMessage is "++
    --   (show $ fmap (\l -> (psMessage (rangesForLn l (maybe S.empty id mranges))
    --                        (getIrssiMessageText $ _llogType l))) allLns)
                          
                           --(fmap (getIrssiMessageText . _llogType) (S.index allLns l)))) [srow..(srow+cnt-1)])

    let
      srow' = if srow < 0 then (Prelude.length allLns) - cnt else srow
      lns = sliceSeq  srow' cnt allLns
      in
      do
        -- lift $ logError $ encodeUtf8 $ T.pack $  "srow' is "++(show srow')
        -- lift $ logError $ encodeUtf8 $ T.pack $  "lns is "++(show lns)
        if (Prelude.length lns) == 0  then (EitherT $ return (Left "No lines")) else return ()
        lift $ renderWithSplices "loadRows" $
          do
            "Rows" ## (return $ mapInd (lineToRow srow' $ maybe S.empty id mranges) $ toList lns)
            "Date" ## (return [X.Element "Date" [] [X.TextNode $ pack $ show $
                                                    _ltime $ S.index lns 0 ]])

  where
    lineToRow :: Int -> Seq Range -> ILine -> Int -> X.Node
    lineToRow indexOffset r l i =  
        X.Element "Row" [("id",pack $ show (i + indexOffset))] $
           prettyShowLT (_llogType l) (i + indexOffset) r
    getRangesForKeyword :: IState -> Text -> Seq Range
    getRangesForKeyword s k = _cranges $ Prelude.head (Prelude.filter (\cir -> k == (_idisplayName (_cindex cir))) (_scirs s))
    
--show the logtype in a pretty fashion
prettyShowLT :: LogType -> Int -> Seq Range -> [X.Node]
prettyShowLT lt lineNumber ranges =
  psType lt (psMessage (rangesForLn lineNumber ranges))
  where
    psType :: LogType -> (Text -> [X.Node]) -> [X.Node]
    psType (Join _ nick _ mc) msgElem = [selem "join",nickElem nick,X.TextNode " "] ++ msgElem mc
    psType (Part _ nick _ mc) msgElem = [selem "part",nickElem nick,X.TextNode " "] ++ msgElem mc
    psType (Quit _ nick _ mc) msgElem = [selem "quit",nickElem nick,X.TextNode " "] ++ msgElem mc
    psType (Kick _ nick k mc) msgElem = [selem "kick",nickElem nick, nickElem k,X.TextNode " "] ++ msgElem mc
    psType (Message _ _ nick mc) msgElem = [selem "msg",nickElem nick,X.TextNode ": "] ++ msgElem mc
    psType (Action _ nick mc) msgElem = [selem "action",nickElem nick,X.TextNode " "] ++ msgElem mc
    psType lt' _ = [selem $ T.pack (show lt')]


    nickElem :: Text -> X.Node
    nickElem n = X.Element "span" [("class","nick")] [X.TextNode n]
    selem :: Text -> X.Node
    selem t = X.Element "span" [("class",t)] []
    
psMessage :: [(Int,Int)] -> Text -> [X.Node]
psMessage rngs t =
      doit rngs 0
      where
        --go through ranges and text, creating nodes as necessary
        --r - ranges to highlight (start pos, end pos exclusive)
        --i - index within text
        doit :: [(Int,Int)] -> Int -> [X.Node]
        doit _ i
          | i == (T.length t) = [] --no more text
          | i > (T.length t) = undefined
        doit [] i = [X.TextNode (T.drop i t)]  --no more ranges
        doit ars@((spos,epos) : rs) i 
          | i < spos = -- text before next range start
            let nt = T.drop i (T.take spos t)
            in X.TextNode nt : doit ars spos
          | i == spos =  --text at next range start (should never be past it)
            let nt = T.drop spos (T.take epos t)
            in X.Element "span" [("class","keyword")] [X.TextNode nt] : doit rs epos
          | otherwise = undefined


rangesForLn :: Int -> Seq Range -> [(Int, Int)]
rangesForLn ln rngs 
  | (S.null rngs) = []
  | otherwise =
          let maybeRangesSR = SE.lookupRanges True $ SE.search (SE.fromTo 0 $ (S.length rngs)-1) SE.divForever (\v -> ln <= (_prow $ _rstartPos $ S.index rngs v))
              myRanges' = case maybeRangesSR of
                Just rsr -> toList $ S.takeWhileL (\r -> (_prow $ _rstartPos $ r) == ln) (S.drop (SE.loVal $ rsr) rngs)
                Nothing -> []
          in
            Prelude.fmap (\r -> (_pcol $ _rstartPos r, _pcol $ _rendPos r)) myRanges'


_testRanges :: Seq Range
_testRanges = S.fromList $ fmap (\l -> Range "foo" (Pos l l) (Pos l (l+2))) [0..10]
_testRanges2 :: Seq Range
_testRanges2 = S.fromList $ fmap (\l -> Range "foo" (Pos (div l 2) $ l * 3) (Pos (div l 2) (l*3+2))) [0..10]
              
_test :: IO ()
_test =
  hspec $ do
  describe "psMessage" $ do
    it "embedded keyword" $ do
      (psMessage (rangesForLn 0 (S.fromList [Range "foo" (Pos 0 2) (Pos 0 4)]))  "abcdefghi")
        `shouldBe` [X.TextNode "ab",X.Element {X.elementTag = "span", X.elementAttrs = [("class","keyword")], X.elementChildren = [X.TextNode "cd"]},X.TextNode "efghi"]
    it "starts with keyword" $ do
      (psMessage (rangesForLn 0 (S.fromList [Range "foo" (Pos 0 0) (Pos 0 4)]))  "abcdefghi")
        `shouldBe` [X.Element {X.elementTag = "span", X.elementAttrs = [("class","keyword")], X.elementChildren = [X.TextNode "abcd"]},X.TextNode "efghi"]
    it "ends with keyword" $ do        
      (psMessage (rangesForLn 0 (S.fromList [Range "foo" (Pos 0 5) (Pos 0 9)]))  "abcdefghi")
        `shouldBe` [X.TextNode "abcde",X.Element {X.elementTag = "span", X.elementAttrs = [("class","keyword")], X.elementChildren = [X.TextNode "fghi"]}]
    it "handles ranges outside of line" $ do        
      (psMessage (rangesForLn 5 (S.fromList [Range "foo" (Pos 0 8) (Pos 0 9),
                                Range "foo" (Pos 5 5) (Pos 5 9)
                               ,Range "foo" (Pos 8 2) (Pos 8 9)
                               ]))  "abcdefghi")
        `shouldBe` [X.TextNode "abcde",X.Element {X.elementTag = "span", X.elementAttrs = [("class","keyword")], X.elementChildren = [X.TextNode "fghi"]}]
    it "handles being first range with ranges outside of line" $ do        
      (psMessage (rangesForLn 0 (S.fromList [Range "foo" (Pos 0 5) (Pos 0 9),
                                Range "foo" (Pos 5 3) (Pos 5 9)
                               ,Range "foo" (Pos 8 1) (Pos 8 9)
                               ]))  "abcdefghi")
        `shouldBe` [X.TextNode "abcde",X.Element {X.elementTag = "span", X.elementAttrs = [("class","keyword")], X.elementChildren = [X.TextNode "fghi"]}]
    it "handles being last range with ranges outside of line" $ do        
      (psMessage (rangesForLn 8 (S.fromList [Range "foo" (Pos 0 0) (Pos 0 9),
                                Range "foo" (Pos 5 3) (Pos 5 9)
                               ,Range "foo" (Pos 8 5) (Pos 8 9)
                               ]))  "abcdefghi")
        `shouldBe` [X.TextNode "abcde",X.Element {X.elementTag = "span", X.elementAttrs = [("class","keyword")], X.elementChildren = [X.TextNode "fghi"]}]
    it "handles multiple ranges in one line" $ do        
      (psMessage (rangesForLn 1 _testRanges2)  "abcdefghijklmnopqrstuvwxyz")
        `shouldBe` [X.TextNode "abcdef",
                    X.Element {X.elementTag = "span", X.elementAttrs = [("class","keyword")], X.elementChildren = [X.TextNode "gh"]},
                    X.TextNode "i",
                    X.Element {X.elementTag = "span", X.elementAttrs = [("class","keyword")], X.elementChildren = [X.TextNode "jk"]},
                    X.TextNode "lmnopqrstuvwxyz"]
        
    
  

