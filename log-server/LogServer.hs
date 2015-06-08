module LogServer where

import Configuration
import Control.Exception.Lifted
import Control.Monad.Base
import Data.Default
import Data.String
import Database.PostgreSQL.PQTypes
import Log
import Log.Backend.StandardOutput
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Data.Text.Encoding as T

import Handlers
import LogServerConf
import SQL

type MainM = LogT IO

main :: IO ()
main = do
  conf <- readConfig putStrLn "log_server.conf"
  logger <- stdoutLogger
  (`finally` waitForLogger logger) . withLogger logger $ do
    pool <- liftBase $ poolSource (def {
      csConnInfo = T.encodeUtf8 $ lscDBConfig conf
    }) 1 10 10
    startServer logger pool conf
  where
    withLogger :: Logger -> LogT m r -> m r
    withLogger = runLogT "log-server"

    startServer :: Logger -> ConnectionSource -> LogServerConf -> MainM ()
    startServer logger pool LogServerConf{..} = do
      let ss = setHost (fromString lscBindHost)
             . setPort (fromIntegral lscBindPort)
             . setOnExceptionResponse apiError
             . setOnException (handleServerError logger)
             $ defaultSettings
      liftBase . runSettings ss $ appHandler $ withLogger logger . runDB pool

    handleServerError :: Logger -> Maybe Request -> SomeException -> IO ()
    handleServerError logger rq err = withLogger logger $ do
      logAttention "Server error" $ object [
          "request" .= show rq
        , "error" .= show err
        ]
