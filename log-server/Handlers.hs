module Handlers where

import Control.Applicative
import Control.Concurrent.Lifted
import Control.Monad.Catch
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Foldable (foldMap)
import Data.Monoid
import Database.PostgreSQL.PQTypes
import Happstack.Server hiding (body, dir, path)
import Happstack.Server.Instances.Overlapping ()
import Happstack.Server.ReqHandler
import Happstack.StaticRouting
import Log
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as V

import SQL

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

----------------------------------------

-- Workaround for broken 'ToMessage' instances in Happstack.
class ToBS t where
  toBS :: t -> BSL.ByteString

instance ToBS BSL.ByteString where
  toBS = id

instance ToBS Value where
  toBS = encode

api :: ToBS t => HandlerM t -> HandlerM Response
api m = mask_ $ try m >>= \case
  Right value -> apiOk value
  Left (e::SomeException) -> apiError $ show e

apiOk :: ToBS t => t -> HandlerM Response
apiOk = ok . toResponseBS jsonContentType . toBS

apiError :: String -> HandlerM Response
apiError = badRequest . toResponseBS jsonContentType . encode . f
  where
    f msg = object ["error" .= msg]

----------------------------------------

handleApiComponents :: HandlerM Response
handleApiComponents = api $ do
  runSQL_ "SELECT DISTINCT component FROM logs ORDER BY component"
  components <- V.fromList <$> fetchMany (String . runIdentity)
  return $ object ["components" .= Array components]

handleApiLogs :: HandlerM Response
handleApiLogs = api $ askRq >>= tryTakeMVar . rqBody >>= \case
  Nothing -> error "handleApiLogs: no request body"
  Just (Body body) -> do
    runSQL_ . sqlSelectLogs $ parseLogRequest body
    -- Exploit lazy evaluation. More memory efficient than
    -- feeding the list of messages directly into Aeson.
    qr <- fmap fetchLog <$> queryResult
    return . BSB.toLazyByteString
           . wrap
           $ foldMap (BSB.lazyByteString . encode) qr
  where
    wrap = (BSB.byteString "{\"logs\":[" <>) . (<> BSB.byteString "]}")
