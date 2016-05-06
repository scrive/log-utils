
module SQL (
    runDB
  , fetchComponents
  , LogRequest(..)
  , defLogLimit
  , parseLogRequest
  , foldChunkedLogs
  ) where

import Control.Applicative
import Control.Exception (ErrorCall(..))
import Control.Monad.Base
import Control.Monad.Catch
import Data.Aeson
import Data.Functor.Invariant
import Data.List
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
, lrFrom      :: !(Maybe UTCTime)
, lrTo        :: !(Maybe UTCTime)
, lrWhere     :: !(Maybe (RawSQL ()))
, lrLimit     :: !(Maybe Int)
, lrLast      :: !Bool
}

defLogLimit :: Int
defLogLimit = 1000

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
  <*> fieldOpt "from"
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
  <*> field "last"
      lrLast
      "if true, fetch <LIMIT> last logs, otherwise first ones"

----------------------------------------

foldChunkedLogs :: (MonadBase IO m, MonadDB m, MonadMask m)
                => LogRequest
                -> (acc -> m acc)
                -> acc
                -> (acc -> QueryResult (UTCTime, LogMessage) -> m acc)
                -> m acc
foldChunkedLogs req betweenChunks initAcc f = do
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
  bracket_ declare close $ loop cursor True initAcc
  where
    loop cursor firstPass acc = do
      n <- runSQL $ "FETCH FORWARD 100 FROM" <+> cursor
      if n == 0
        then return acc
        else do
          acc' <- if firstPass
            then return acc
            else betweenChunks acc
          queryResult
            >>= f acc' . fmap fetchLog
            >>= loop cursor False

sqlSelectLogs :: LogRequest -> SQL
sqlSelectLogs LogRequest{..} = smconcat [
    "WITH filtered_logs AS ("
  , "SELECT" <+> concatComma (unique $ logsFields ++ orderFields)
  , "FROM logs"
  , "WHERE" <+> (mintercalate " AND " $ catMaybes [
      Just "TRUE"
    , ("time >" <?>) <$> lrFrom
    , ("time <=" <?>) <$> lrTo
    , ("component =" <?>) <$> lrComponent
    , raw <$> lrWhere
    ])
  , "ORDER BY" <+> (concatComma $ map (<+> orderType) orderFields)
  -- Limit the amount to be fetched. To be honest, browsers
  -- will probably blow up when they receive 10k records anyway.
  , "LIMIT" <?> fromMaybe defLogLimit lrLimit
  , ")"
  , "SELECT" <+> concatComma logsFields
  , "FROM filtered_logs"
  , "ORDER BY" <+> concatComma orderFields
  ]
  where
    unique :: Ord a => [a] -> [a]
    unique = map head . group . sort

    concatComma :: [RawSQL ()] -> SQL
    concatComma = mintercalate ", " . map raw

    orderFields :: [RawSQL ()]
    orderFields = [
        "time"
      , "insertion_time"
      , "insertion_order"
      ]
    orderType :: RawSQL ()
    orderType = if lrLast
      then "DESC"
      else "ASC"

    logsFields :: [RawSQL ()]
    logsFields = [
        "insertion_time"
      , "time"
      , "level"
      , "component"
      , "domain"
      , "message"
      , "data"
      ]

fetchLog :: (UTCTime, UTCTime, T.Text, T.Text, Array1 T.Text, T.Text, JSONB Value)
         -> (UTCTime, LogMessage)
fetchLog (insertion_time, time, level, component, Array1 domain, message, JSONB data_) =
  (insertion_time, LogMessage {
      lmComponent = component
    , lmDomain    = domain
    , lmTime      = time
    , lmLevel     = readLogLevel level
    , lmMessage   = message
    , lmData      = data_
    })
