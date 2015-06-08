module LogFetcher where

import Control.Monad.Base
import Data.Default
import Database.PostgreSQL.PQTypes
import Log.Data
import System.Environment
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Foldable as F
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

import SQL

main :: IO ()
main = do
  -- TODO: get arguments in a more sophisticated manner.
  [connInfo, strRq] <- getArgs
  runDB (simpleSource $ def { csConnInfo = toBS connInfo }) $ do
    logRq <- parseLogRequest (BSL.fromStrict $ toBS strRq)
    withChunkedLogs logRq $ F.mapM_ (liftBase . T.putStrLn . showLogMessage)
  where
    toBS :: String -> BS.ByteString
    toBS = T.encodeUtf8 . T.pack
