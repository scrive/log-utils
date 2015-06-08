module LogServerConf (
    LogServerConf(..)
  ) where

import Control.Applicative
import Data.Default
import Data.Unjson
import Data.Word
import qualified Data.Text as T

data LogServerConf = LogServerConf {
  lscBindHost :: !String
, lscBindPort :: !Word16
, lscDBConfig :: !T.Text
}

instance Unjson LogServerConf where
  unjsonDef = objectOf $ LogServerConf
    <$> field "bind_ip"
        lscBindHost
        "IP to listen on, defaults to 127.0.0.1 (see http://hackage.haskell.org/package/warp/docs/Network-Wai-Handler-Warp.html#t:HostPreference for more information)"
    <*> field "bind_port"
        lscBindPort
        "Port to listen on"
    <*> fieldBy "database"
        lscDBConfig
        "Database connection string"
        unjsonAeson

instance Default LogServerConf where
  def = LogServerConf {
    lscBindHost = "127.0.0.1"
  , lscBindPort = 7000
  , lscDBConfig = "user='kontra' password='kontra' dbname='kontrakcja'"
  }
