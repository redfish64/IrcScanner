module Learn.HeistSplices where

import           Heist
import qualified Heist.Compiled as C
import qualified Heist.Interpreted as I

import           Blaze.ByteString.Builder
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import qualified Control.Monad.Trans.State as ST
import           Data.ByteString.Char8 (ByteString)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import           Control.Applicative
import           Control.Lens
import           Data.Map.Syntax
import Control.Monad.Trans.Either

main :: IO ()
main = undefined
    

-- names :: IO ()
-- names = eitherT (putStrLn . unlines) return $ do
--   heist <- initHeist mempty
--     { hcTemplateLocations = [ loadTemplates "templates" ]
--     , hcInterpretedSplices = defaultInterpretedSplices
--     }

--   names <- liftIO getNames

--   forM_ names $ \name -> do
--     Just (output, _) <- renderTemplate
--       (bindSplice "kiddo" (textSplice name) heist)
--       "merry-christmas"

--     liftIO . putStrLn . toByteString $ output
    
