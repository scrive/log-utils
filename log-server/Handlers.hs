module Handlers (
    apiError
  , appHandler
  ) where

import Control.Applicative
import Control.Exception (SomeException)
import Control.Monad.Base
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Monoid.Utils
import Database.PostgreSQL.PQTypes
import Log
import Network.HTTP.Types
import Network.Wai
import qualified Data.ByteString.Builder as BSB
import qualified Data.Foldable as F
import qualified Data.Vector as V

import SQL

type HandlerM = DBT (LogT IO)
type HandlerRunner = forall r. HandlerM r -> IO r

data HandlerEnv = HandlerEnv {
  heRunHandler :: !HandlerRunner
, heRequest    :: !Request
, heRespond    :: !(Response -> IO ResponseReceived)
}

apiError :: SomeException -> Response
apiError = responseLBS badRequest400 [(hContentType, jsonContentType)] . encode . f
  where
    f err = object ["error" .= show err]

jsonContentType :: ByteString
jsonContentType = "application/json"

----------------------------------------

appHandler :: HandlerRunner -> Application
appHandler runHandler rq respond = case pathInfo rq of
  -- We could use wai-routes for that, but there really is no point
  -- as these routes are very simple and won't get more complicated.
  ["api", "components"] | isGET -> handleApiComponents env
  ["api", "logs"] | isPOST -> handleApiLogs env
  _ -> respond $ responseLBS notFound404 [] "Nothing is here."
  where
    isGET  = requestMethod rq == methodGet
    isPOST = requestMethod rq == methodPost

    env = HandlerEnv {
      heRunHandler = runHandler
    , heRequest = rq
    , heRespond = respond
    }

handleApiComponents :: HandlerEnv -> IO ResponseReceived
handleApiComponents HandlerEnv{..} = do
  components <- heRunHandler $ V.fromList . map String <$> fetchComponents
  heRespond . responseOk . encode $ object ["components" .= Array components]
  where
    responseOk = responseLBS ok200 [(hContentType, jsonContentType)]

handleApiLogs :: HandlerEnv -> IO ResponseReceived
handleApiLogs HandlerEnv{..} = do
  logRq <- parseLogRequest =<< lazyRequestBody heRequest
  heRespond . responseOk $ \write _flush -> heRunHandler $ do
    liftBase $ write "{\"logs\":["
    withChunkedLogs logRq (liftBase $ write ",") $ \qr -> liftBase $ do
      write . mintercalate (BSB.lazyByteString ",")
            . map (BSB.lazyByteString . encode)
            $ F.toList qr
    liftBase $ write "]}"
  where
    responseOk = responseStream ok200 [(hContentType, jsonContentType)]
