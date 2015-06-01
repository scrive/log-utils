module Handlers where

import Control.Applicative
import Control.Concurrent.Lifted
import Control.Exception (ErrorCall(..))
import Control.Monad.Catch
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Functor.Invariant
import Data.Maybe
import Data.Monoid.Utils
import Data.Unjson
import Database.PostgreSQL.PQTypes
import Happstack.Server hiding (body, dir, path)
import Happstack.Server.Instances.Overlapping ()
import Happstack.Server.ReqHandler
import Happstack.StaticRouting
import Log
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

type HandlerM = DBT (LogT (ReqHandlerT IO))

handlers :: Route (HandlerM Response)
handlers = choice [
    dir "api" $ choice [
      dir "components" $ hGet  $ handleApiComponents
    , dir "logs"       $ hPost $ handleApiLogs
    ]
  ]
  where
    hGet  = path GET id
    hPost = path POST id

jsonContentType :: ByteString
jsonContentType = "application/json"

apiOk :: Value -> HandlerM Response
apiOk = ok . toResponseBS jsonContentType . encode

apiError :: String -> HandlerM Response
apiError = badRequest . toResponseBS jsonContentType . encode . f
  where
    f msg = object ["error" .= msg]

api :: HandlerM Value -> HandlerM Response
api m = mask_ $ try m >>= \case
  Right value -> apiOk value
  Left (e::SomeException) -> apiError $ show e

handleApiComponents :: HandlerM Response
handleApiComponents = api $ do
  runSQL_ "SELECT DISTINCT component FROM logs ORDER BY component"
  components <- V.fromList <$> fetchMany (String . runIdentity)
  return $ object ["components" .= Array components]

data LogRequest = LogRequest {
  lrComponent :: !(Maybe T.Text)
, lrFrom      :: !UTCTime
, lrTo        :: !(Maybe UTCTime)
, lrWhere     :: !(Maybe (RawSQL ()))
}

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

handleApiLogs :: HandlerM Response
handleApiLogs = api $ askRq >>= tryTakeMVar . rqBody >>= \case
  Nothing -> errorM "no request body"
  Just (Body body) -> case eitherDecode body of
    Left err -> errorM $ "Aeson:" <+> err
    Right value -> case parse unjsonLogRequest value of
      Result LogRequest{..} [] -> do
        runSQL_ $ smconcat [
            "SELECT time, level, component, domain, message, data"
          , "FROM logs"
          , "WHERE time >=" <?> lrFrom
          , mintercalate "AND" $ catMaybes [
              ("time <=" <?>) <$> lrTo
            , ("component =" <?>) <$> lrComponent
            , raw <$> lrWhere
            ]
          , "ORDER BY time, insertion_time, insertion_order"
          -- Limit the amount to be fetched. To be honest, browsers
          -- will probably blow up when they receive 10k records anyway.
          , "LIMIT 10000"
          ]
        logs <- V.fromList <$> fetchMany fetchLog
        return $ object ["logs" .= Array logs]
      Result _ errs -> errorM . unlines $ map show errs
  where
    errorM = throwM . ErrorCall . ("handleApiLogs" ++)

fetchLog :: (UTCTime, T.Text, T.Text, T.Text, T.Text, JSONB Value) -> Value
fetchLog (time, level, component, domain, message, JSONB data_) = object [
    "time" .= time
  , "level" .= level
  , "component" .= component
  , "domain" .= domain
  , "message" .= message
  , "data" .= data_
  ]
