module SQL (
    runDB
  , fetchComponents
  , LogRequest(..)
  , defLogLimit
  , parseLogRequest
  , withChunkedLogs
  ) where

import Control.Applicative
import Control.Exception (ErrorCall(..))
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Data.Aeson
import Data.Function
import Data.Functor.Invariant
import Data.Maybe
import Data.Monoid
import Data.Monoid.Utils
import Data.Unjson
import Data.Word
import Database.PostgreSQL.PQTypes
import Log
import System.Random
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

runDB :: (MonadBase IO m, MonadMask m) => ConnectionSource -> DBT m r -> m r
runDB cs = runDBT cs def {
  tsPermissions = ReadOnly
}

----------------------------------------

fetchComponents :: MonadDB m => m [T.Text]
fetchComponents = do
  runSQL_ "SELECT DISTINCT component FROM logs ORDER BY component"
  fetchMany runIdentity

----------------------------------------

data LogRequest = LogRequest {
  lrComponent :: !(Maybe T.Text)
, lrFrom      :: !UTCTime
, lrTo        :: !(Maybe UTCTime)
, lrWhere     :: !(Maybe (RawSQL ()))
, lrLimit     :: !(Maybe Int)
}

defLogLimit :: Int
defLogLimit = 10000

parseLogRequest :: MonadThrow m => BSL.ByteString -> m LogRequest
parseLogRequest s = case eitherDecode s of
  Left err -> throwE $ "aeson:" <+> err
  Right value -> case parse unjsonLogRequest value of
    Result req [] -> return req
    Result _ errs -> throwE . unlines $ map show errs
  where
    throwE = throwM . ErrorCall . ("parseLogRequest: " ++)

unjsonLogRequest :: UnjsonDef LogRequest
unjsonLogRequest = objectOf $ LogRequest
  <$> fieldOpt "component"
      lrComponent
      "system component"
  <*> field "from"
      lrFrom
      "fetch logs since"
  <*> fieldOpt "to"
      lrTo
      "fetch logs until"
  <*> fieldOptBy "where"
      lrWhere
      "sql where clause"
      -- This conversion is obviously unsafe, but the assumption
      -- is that we will use database user with constrained access
      -- rights, so it shouldn't be a problem.
      (invmap ((`rawSQL` ()) . T.encodeUtf8) (T.decodeUtf8 . unRawSQL) unjsonAeson)
  <*> fieldOpt "limit"
      lrLimit
      ("limit of logs (defaults to " <> T.pack (show defLogLimit) <> ")")

----------------------------------------

withChunkedLogs :: (MonadBase IO m, MonadDB m, MonadMask m)
                => LogRequest -> m () -> (QueryResult LogMessage -> m ()) -> m ()
withChunkedLogs req betweenChunks f = do
  uuid :: Word32 <- liftBase randomIO
  -- Stream logs from the database using a cursor.
  let cursor = "log_fetcher_" <> unsafeSQL (show uuid)
      declare = runSQL_ $ smconcat [
          "DECLARE"
        , cursor
        , "NO SCROLL CURSOR FOR"
        , sqlSelectLogs req
        ]
      close = runSQL_ $ "CLOSE" <+> cursor
  bracket_ declare close . (`fix` False) $ \loop notFirst -> do
    n <- runSQL $ "FETCH FORWARD 100 FROM" <+> cursor
    when (n > 0) $ do
      when notFirst betweenChunks
      f . fmap fetchLog =<< queryResult
      loop True

sqlSelectLogs :: LogRequest -> SQL
sqlSelectLogs LogRequest{..} = smconcat [
    "SELECT time, level, component, domain, message, data"
  , "FROM logs"
  , "WHERE time >=" <?> lrFrom <+> "AND"
  , mintercalate " AND " $ catMaybes [
      Just "TRUE"
    , ("time <=" <?>) <$> lrTo
    , ("component =" <?>) <$> lrComponent
    , raw <$> lrWhere
     ]
  , "ORDER BY time, insertion_time, insertion_order"
  -- Limit the amount to be fetched. To be honest, browsers
  -- will probably blow up when they receive 10k records anyway.
  , "LIMIT" <?> fromMaybe defLogLimit lrLimit
  ]

fetchLog :: (UTCTime, T.Text, T.Text, Array1 T.Text, T.Text, JSONB Value)
         -> LogMessage
fetchLog (time, level, component, Array1 domain, message, JSONB data_) =
  LogMessage {
    lmComponent = component
  , lmDomain    = domain
  , lmTime      = time
  , lmLevel     = readLogLevel level
  , lmMessage   = message
  , lmData      = data_
  }
