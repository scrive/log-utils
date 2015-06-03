module SQL (
    tsRO
  , LogRequest(..)
  , parseLogRequest
  , streamLogs
  ) where

import Control.Applicative
import Control.Concurrent.Lifted
import Control.Exception.Lifted
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Trans.Control
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
import System.IO.Unsafe
import System.Random
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

tsRO :: TransactionSettings
tsRO = def {
  tsPermissions = ReadOnly
}

----------------------------------------

data LogRequest = LogRequest {
  lrComponent :: !(Maybe T.Text)
, lrFrom      :: !UTCTime
, lrTo        :: !(Maybe UTCTime)
, lrWhere     :: !(Maybe (RawSQL ()))
, lrLimit     :: !(Maybe Int)
}

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
      "limit of logs (defaults to 10000)"

----------------------------------------

-- | Like 'getChanContents', but allows for the list to end.
varToStream :: MVar (Maybe a) -> IO [a]
varToStream mv = unsafeInterleaveIO $ takeMVar mv >>= \case
  Just e  -> (e :) <$> varToStream mv
  Nothing -> return []

----------------------------------------

-- | Return lazy list of logs streamed from the database.
-- Note that if receiving thread won't process the whole
-- list and exit, the producer will receive exception
-- 'BlockedIndefinitelyOnMVar' and exit cleanly.
streamLogs :: (MonadDB m, MonadBaseControl IO m) => LogRequest -> m [LogMessage]
streamLogs req = do
  mv <- newEmptyMVar
  void . fork . withNewConnection $ do
    uuid :: Word32 <- liftBase randomIO
    -- Stream logs from the database using a cursor.
    let endStream = putMVar mv Nothing
        cursor = "log_fetcher_" <> unsafeSQL (show uuid)
        declare = runSQL_ $ smconcat [
            "DECLARE"
          , cursor
          , "NO SCROLL CURSOR FOR"
          , sqlSelectLogs req
          ]
        close = runSQL_ $ "CLOSE" <+> cursor
    (`finally` endStream) . bracket_ declare close . fix $ \loop -> do
      n <- runSQL $ "FETCH FORWARD 1000 FROM" <+> cursor
      when (n > 0) $ do
        mapDB_ $ putMVar mv . Just . fetchLog
        loop
  liftBase $ varToStream mv
  where
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
      , "LIMIT" <?> fromMaybe 10000 lrLimit
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
