module LogFetcher where

import Control.Monad.Base
import Data.Default
import Database.PostgreSQL.PQTypes
import Log.Data
import System.Environment
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T

import SQL

main :: IO ()
main = do
  -- TODO: get arguments in a more sophisticated manner.
  [ci, sreq] <- getArgs
  runDBT (simpleSource $ def { csConnInfo = toBS ci }) tsRO $ do
    parseLogRequest (BSL.fromStrict $ toBS sreq)
      >>= streamLogs
      >>= mapM_ (liftBase . T.putStrLn . showLogMessage)
  where
    toBS :: String -> BS.ByteString
    toBS = T.encodeUtf8 . T.pack
