module Parser.Irssi.Log.Streaming.Raw (
  rawToLog
) where

import           Pipes
import qualified Pipes.Prelude as P
import           Pipes.Safe
import qualified Pipes.Text    as Text
import qualified Pipes.Text.IO as Text



rawToLog = undefined
  -- results <- runSafeT $ runEffect $ Text.readFile path >-> P.map parseRawMessageText >-> drain
