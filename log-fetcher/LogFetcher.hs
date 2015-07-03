module LogFetcher where

import Control.Exception (ErrorCall(..))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Base
import Data.Aeson
import Data.Default
import Data.Maybe
import Data.Time
import Database.PostgreSQL.PQTypes
import Log.Data
import System.Environment
import System.Console.CmdArgs.Implicit hiding (def)
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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

main :: IO ()
main = do
  progName <- getProgName
  cmd <- cmdArgs $ modes [cmdComponents, cmdLogs] &= program progName
  let cs = simpleSource $ def { csConnInfo = toBS $ database cmd }
  case cmd of
    Components{..} -> runDB cs $ fetchComponents >>= mapM_ (liftBase . T.putStrLn)
    Logs{..} -> runDB cs $ do
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
      n <- foldChunkedLogs logRq return (0::Int) $ \n qr -> liftBase $ do
        F.mapM_ (T.putStrLn . showLogMessage) qr
        return $! n + ntuples qr
      liftBase $ putStrLn $ show n ++ " log messages fetched."
  where
    toBS :: String -> BS.ByteString
    toBS = T.encodeUtf8 . T.pack

    parseTime_ :: MonadThrow m => String -> m UTCTime
    parseTime_ s = case mtime of
      Just time -> return time
      Nothing   -> throwM . ErrorCall $ "parseTime_: invalid value: " ++ s
      where
        mtime = msum [
            parse "%Y-%m-%d" s
          , parse "%Y-%m-%d %H:%M:%S%Q" s
          ]
        parse = parseTimeM True defaultTimeLocale
