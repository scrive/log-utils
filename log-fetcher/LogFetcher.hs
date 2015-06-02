module LogFetcher where

import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Data.Default
import Data.Function
import Data.Monoid
import Data.Monoid.Utils
import Data.Word
import Database.PostgreSQL.PQTypes
import Log.Data
import System.Environment
import System.Random
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T

import SQL

main :: IO ()
main = do
  -- TODO: something more sophisticated should be done here.
  [ci, sreq] <- getArgs
  uuid :: Word32 <- randomIO
  runDBT (simpleSource $ def { csConnInfo = toBS ci }) ts $ do
    -- Stream logs from the database using a cursor.
    let cursor = "log_fetcher_" <> unsafeSQL (show uuid)
        declare = runSQL_ $ smconcat [
            "DECLARE"
          , cursor
          , "NO SCROLL CURSOR FOR"
          , sqlSelectLogs . parseLogRequest . BSL.fromStrict $ toBS sreq
          ]
        close = runSQL_ $ "CLOSE" <+> cursor
    bracket_ declare close . fix $ \loop -> do
      n <- runSQL $ "FETCH FORWARD 1000 FROM" <+> cursor
      when (n > 0) $ do
        mapDB_ $ liftBase . T.putStrLn . showLogMessage . fetchLog
        loop
  where
    toBS :: String -> BS.ByteString
    toBS = T.encodeUtf8 . T.pack

    ts :: TransactionSettings
    ts = def {
      tsPermissions = ReadOnly
    }
