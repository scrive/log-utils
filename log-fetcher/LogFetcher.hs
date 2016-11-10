module Main where

import Control.Applicative
import Control.Concurrent
import Control.Exception (ErrorCall(..))
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Trans.Reader
import Data.Aeson
import Data.Default
import Data.Function
import Data.IORef
import Data.Maybe
import Data.Time
import Database.PostgreSQL.PQTypes
import Log.Data
import Prelude
import System.Console.CmdArgs.Implicit hiding (def)
import System.Environment
import qualified Data.Foldable as F
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Traversable as T

import SQL

data CmdArgument = Logs {
  database  :: String
, component :: Maybe String
, from      :: Maybe String
, to        :: Maybe String
, where_    :: Maybe String
, limit     :: Maybe Int
, last_     :: Bool
, follow    :: Bool
} | Components {
  database :: String
} deriving (Data, Typeable)

cmdLogs :: CmdArgument
cmdLogs = Logs {
  database  = defDatabase
, component = Nothing
           &= name "c"
           &= help "system component (optional)"
           &= typ "COMPONENT"
, from      = Nothing
           &= name "f"
           &= help ("fetch logs since (optional, format: " ++ timeFormat ++ ")")
           &= typ "TIMESTAMP"
, to        = Nothing
           &= name "t"
           &= help ("fetch logs until (optional, format: " ++ timeFormat ++ ")")
           &= typ "TIMESTAMP"
, where_    = Nothing
           &= name "w"
           &= help "WHERE clause to further filter logs (optional)"
           &= typ "SQL"
, limit     = Nothing
           &= name "l"
           &= help ("limit of fetched logs (optional, default: " ++ show defLogLimit ++ ")")
, last_     = False
           &= help "fetch <LIMIT> last logs instead of first ones (optional)"
, follow    = False
           &= help "output logs as they are recorded (with 5 seconds delay). If set, implicitly enables 'last' and overwrites 'to' (optional)"
} &= help "Fetch the list of log messages fulfilling set criteria"
  where
    timeFormat = "'YYYY-MM-DD hh:mm:ss'"

cmdComponents :: CmdArgument
cmdComponents = Components {
  database = defDatabase
} &= help "Fetch the list of available components"

defDatabase :: String
defDatabase = ""
           &= name "d"
           &= help "database connection info (required)"
           &= typ "CONNINFO"

----------------------------------------

data LogsSt = LogsSt {
  lsLogNumber :: !(IORef Int)
}

type LogsM = ReaderT LogsSt (DBT IO)

main :: IO ()
main = do
  progName <- getProgName
  cmd <- cmdArgs $ modes [cmdComponents, cmdLogs] &= program progName
  let cs = simpleSource $ def { csConnInfo = T.pack $ database cmd }
  case cmd of
    Components{..} -> runDB (unConnectionSource cs) $ fetchComponents >>= mapM_ (liftBase . T.putStrLn)
    Logs{..} -> runDB (unConnectionSource cs) $ do
      utcFrom <- T.mapM parseTime_ from
      utcTo <- T.mapM parseTime_ to
      logRq <- parseLogRequest . encode . object $ catMaybes [
          fmap ("component" .=) component
        , fmap ("from"      .=) utcFrom
        , fmap ("to"        .=) utcTo
        , fmap ("where"     .=) where_
        , fmap ("limit"     .=) limit
        , Just $ "last" .= last_
        ]
      initSt <- LogsSt <$> liftBase (newIORef 0)
      (`runReaderT` initSt) . (`finally` printSummary) $ if not follow
        then printLogs logRq
        else do
          -- When following logs, start by fetching all logs recorded
          -- until 5 seconds ago and then each second fetch logs recorded
          -- between now - 6s and now - 5s. The delay is needed so that
          -- we don't "lose" logs that might have been recorded with a
          -- slight delay (5 seconds should be plenty of time to archive
          -- that).
          initRq <- liftBase $ getCurrentTime
            >>= \now -> return logRq {
              lrTo   = Just $ fiveSecondsBefore now
            , lrLast = True
            }
          (`fix` initRq) $ \loop rq -> do
            printLogs rq
            liftBase $ threadDelay 1000000
            now <- liftBase getCurrentTime
            loop $ rq {
              lrFrom  = lrTo rq
            , lrTo    = Just $ fiveSecondsBefore now
            , lrLimit = Just maxBound
            }
  where
    fiveSecondsBefore :: UTCTime -> UTCTime
    fiveSecondsBefore t = (-5) `addUTCTime` t

    printSummary :: LogsM ()
    printSummary = do
      n <- liftBase . readIORef =<< asks lsLogNumber
      liftBase $ putStrLn $ show n ++ " log messages fetched."

    printLogs :: LogRequest -> LogsM ()
    printLogs logRq = foldChunkedLogs logRq return () $ \() qr -> do
      liftBase . (`modifyIORef'` (+ ntuples qr)) =<< asks lsLogNumber
      liftBase . F.forM_ qr $ \(t, lm) -> T.putStrLn $ showLogMessage (Just t) lm

    parseTime_ :: MonadThrow m => String -> m UTCTime
    parseTime_ s = case mtime of
      Just time -> return time
      Nothing   -> throwM . ErrorCall $ "parseTime_: invalid value: " ++ s
      where
        mtime = msum [
            parse "%Y-%m-%d" s
          , parse "%Y-%m-%d %H:%M" s
          , parse "%Y-%m-%d %H:%M:%S%Q" s
          ]
        parse = parseTimeM True defaultTimeLocale
