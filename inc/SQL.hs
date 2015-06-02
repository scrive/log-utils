module SQL (
    LogRequest(..)
  , parseLogRequest
  , sqlSelectLogs
  , fetchLog
  ) where

import Control.Applicative
import Data.Aeson
import Data.Functor.Invariant
import Data.Maybe
import Data.Monoid.Utils
import Data.Unjson
import Database.PostgreSQL.PQTypes
import Log
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

data LogRequest = LogRequest {
  lrComponent :: !(Maybe T.Text)
, lrFrom      :: !UTCTime
, lrTo        :: !(Maybe UTCTime)
, lrWhere     :: !(Maybe (RawSQL ()))
, lrLimit     :: !(Maybe Int)
}

parseLogRequest :: BSL.ByteString -> LogRequest
parseLogRequest s = case eitherDecode s of
  Left err -> throwE $ "aeson:" <+> err
  Right value -> case parse unjsonLogRequest value of
    Result req [] -> req
    Result _ errs -> throwE . unlines $ map show errs
  where
    throwE = error . ("parseLogRequest: " ++)

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

sqlSelectLogs :: LogRequest -> SQL
sqlSelectLogs LogRequest{..} = smconcat [
    "SELECT time, level, component, domain, message, data"
  , "FROM logs"
  , "WHERE time >=" <?> lrFrom <+> "AND"
  , mintercalate " AND " $ catMaybes [
      ("time <=" <?>) <$> lrTo
    , ("component =" <?>) <$> lrComponent
    , raw <$> lrWhere
    , Just "TRUE"
    ]
  , "ORDER BY time, insertion_time, insertion_order"
  -- Limit the amount to be fetched. To be honest, browsers
  -- will probably blow up when they receive 10k records anyway.
  , "LIMIT" <?> fromMaybe 10000 lrLimit
  ]

fetchLog :: (UTCTime, T.Text, T.Text, Array1 T.Text, T.Text, JSONB Value) -> LogMessage
fetchLog (time, level, component, Array1 domain, message, JSONB data_) = LogMessage {
  lmComponent = component
, lmDomain = domain
, lmTime = time
, lmLevel = readLogLevel level
, lmMessage = message
, lmData = data_
}
