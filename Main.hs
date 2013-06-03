{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
import Web.Scotty
import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow (ToRow(..))
import Database.PostgreSQL.Simple.ToField (ToField(..))
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Data.Aeson (eitherDecode, FromJSON, ToJSON, parseJSON, toJSON, Value(..))
import Data.Time (LocalTime)
import Data.Time.Format (parseTime, formatTime)
import System.Locale (defaultTimeLocale)
import qualified Data.Text.Lazy as T
import Data.Text (unpack, pack)
import qualified Data.ByteString.Lazy as BL
import Data.Text.Lazy.Encoding (encodeUtf8)

-- Data definitions
data Device = Device {
  mac_addr :: BL.ByteString,
  device_type_id :: Int,
  manufactured_at :: LocalTime,
  registered_at :: Maybe LocalTime
} deriving (Generic, Show)

data Reading = Reading {
  device_mac_addr :: BL.ByteString,
  value :: BL.ByteString,
  created_at :: LocalTime
} deriving (Generic, Show)

-- Aeson data declarations
instance FromJSON Device
instance ToJSON Device
instance FromJSON Reading
instance ToJSON Reading

-- PostgreSQL.Simple data declarations
instance FromRow Device where
  fromRow = Device <$> field <*> field <*> field <*> field

instance ToRow Device where
  toRow Device {mac_addr=addr, device_type_id=dtype, manufactured_at=ma, registered_at=ra} = 
    [toField addr, toField dtype, toField ma, toField ra]

instance FromRow Reading where
  fromRow = Reading <$> field <*> field <*> field

instance ToRow Reading where
  toRow Reading {device_mac_addr=dmac, value=val, created_at=ca} = 
    [toField dmac, toField val, toField ca]

-- Aeson does not define LocalTime translation
instance FromJSON LocalTime where
  parseJSON (String t) = case (parseTime defaultTimeLocale "%F %T%Q" (unpack t) :: Maybe LocalTime) of
                          Nothing -> fail "could not parse local time"
                          Just d -> return d

instance ToJSON LocalTime where
  toJSON t = String $ pack $ formatTime defaultTimeLocale "%F %T%Q" t

-- DB functions
getDevices :: IO [Device]
getDevices = do
  conn <- connect defaultConnectInfo { connectHost = "localhost", connectUser = "sd_ventures", connectPassword = "",
    connectDatabase = "sd_ventures_development" }
  devices <- query_ conn "SELECT mac_addr, device_type_id, manufactured_at, registered_at FROM devices"
  close conn
  return devices


getDevice :: BL.ByteString -> IO [Device]
getDevice mac_addr = do
  conn <- connect defaultConnectInfo { connectHost = "localhost", connectUser = "sd_ventures", connectPassword = "",
    connectDatabase = "sd_ventures_development" }
  devices <- query conn "SELECT mac_addr, device_type_id, manufactured_at, registered_at FROM devices WHERE mac_addr = ?" (Only mac_addr)
  close conn
  return devices


createDevice :: Device -> IO ()
createDevice device = do
  conn <- connect defaultConnectInfo { connectHost = "localhost", connectUser = "sd_ventures", connectPassword = "",
    connectDatabase = "sd_ventures_development" }
  execute conn "INSERT INTO devices (mac_addr, device_type_id, manufactured_at, registered_at) VALUES (?, ?, ?, ?)" device
  close conn
  

getReadings :: BL.ByteString -> IO [Reading]
getReadings device_mac_addr = do
  conn <- connect defaultConnectInfo { connectHost = "localhost", connectUser = "sd_ventures", connectPassword = "",
    connectDatabase = "sd_ventures_development" }
  readings <- query conn "SELECT device_mac_addr, value, created_at FROM readings WHERE device_mac_addr = ?" (Only device_mac_addr)
  close conn
  return readings


createReading :: Reading -> IO ()
createReading reading = do
  conn <- connect defaultConnectInfo { connectHost = "localhost", connectUser = "sd_ventures", connectPassword = "",
    connectDatabase = "sd_ventures_development" }
  execute conn "INSERT INTO readings (device_mac_addr, value, created_at) VALUES (?, ?, ?)" reading
  close conn


-- scotty router
main = scotty 3000 $ do

  get "/api/1/devices" $ do
    devices <- liftIO getDevices :: ActionM [Device]
    json devices

  post "/api/1/devices" $ do
    deviceStr <- body
    let device = eitherDecode deviceStr :: Either String Device 
    case device of
      Left s  -> json (Map.singleton "status" (T.pack s) :: Map.Map T.Text T.Text)
      Right d -> do
                  liftIO $ createDevice d :: ActionM ()
                  json (Map.singleton "status" "ok" :: Map.Map T.Text T.Text)

  get "/api/1/devices/:deviceId" $ do
    deviceId <- param "deviceId" :: ActionM T.Text
    devices <- liftIO $ getDevice (encodeUtf8 deviceId) :: ActionM [Device]
    json $ case devices of
            d : []  -> Just d
            _       -> Nothing

  get "/api/1/devices/:deviceId/readings" $ do
    deviceId <- param "deviceId" :: ActionM T.Text
    readings <- liftIO $ getReadings (encodeUtf8 deviceId) :: ActionM [Reading]
    json readings

  post "/api/1/devices/:deviceId/readings" $ do
    deviceId <- param "deviceId" :: ActionM T.Text
    readingStr <- body
    let reading = eitherDecode readingStr :: Either String Reading
    case reading of
      Left s  -> json (Map.singleton "status" (T.pack s) :: Map.Map T.Text T.Text)
      Right r -> do
                  liftIO $ createReading r :: ActionM ()
                  json (Map.singleton "status" "ok" :: Map.Map T.Text T.Text)
