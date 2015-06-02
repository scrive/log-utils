module LogServer where

import Configuration
import Control.Concurrent.Lifted
import Control.Exception.Lifted
import Control.Monad.Base
import Data.Default
import Database.PostgreSQL.PQTypes
import Happstack.Server
import Log
import Log.Backend.StandardOutput
import LogServerConf
import qualified Data.Text.Encoding as T
import qualified Happstack.StaticRouting as R

import Handlers
import Happstack.Server.ReqHandler

type MainM = LogT IO

main :: IO ()
main = do
  conf <- readConfig putStrLn "log_server.conf"
  logger <- stdoutLogger
  (`finally` waitForLogger logger) . withLogger logger $ do
    pool <- liftBase $ poolSource (def {
      csConnInfo = T.encodeUtf8 $ lscDBConfig conf
    }) 1 10 10
    bracket (startServer logger conf pool) (liftBase . killThread) . const $ do
      liftBase waitForTermination
  where
    withLogger :: forall m r. Logger -> LogT m r -> m r
    withLogger = runLogT "log-server"

    -- perform read only transaction
    ts :: TransactionSettings
    ts = def {
      tsPermissions = ReadOnly
    }

    startServer :: Logger -> LogServerConf -> ConnectionSource -> MainM ThreadId
    startServer logger LogServerConf{..} pool = do
      let handlerConf = nullConf {
            port = fromIntegral lscBindPort
          , logAccess = Nothing
          }
      routes <- case R.compile handlers of
        Left e -> do
          logInfo_ e
          error "startServer: static routing"
        Right r -> return $ r >>= maybe (notFound $ toResponse ("Not found."::String)) return
      socket <- liftBase . bindIPv4 lscBindHost $ fromIntegral lscBindPort
      fork . liftBase . runReqHandlerT socket handlerConf . withLogger logger . runDBT pool ts $ routes
