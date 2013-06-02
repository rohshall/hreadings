{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
import Web.Scotty
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.Text.Lazy as T
import qualified Data.ByteString.Lazy as BL
-- import Data.Text.Lazy.Encoding (decodeUtf8)

data Device = Device {
  deviceTypeId :: Int,
  macAddr :: BL.ByteString
} deriving (Generic, Show)

data Reading = Reading {
  value :: BL.ByteString
} deriving (Generic, Show)

instance FromJSON Device
instance ToJSON Device
instance FromJSON Reading
instance ToJSON Reading

main = scotty 3000 $ do

  get "/api/1/devices" $ do
    json $ [Device {deviceTypeId=10, macAddr="Impedence Measurement Device"},
      Device{deviceTypeId=20, macAddr="NIBP measurement device"}]

  post "/api/1/devices" $ do
    deviceStr <- body
    let device = decode deviceStr :: Maybe Device
    json $ case device of
            Just d  -> Map.singleton "status" "ok" :: Map.Map T.Text T.Text
            Nothing -> Map.singleton "status" "wrong device parameters" :: Map.Map T.Text T.Text

  get "/api/1/devices/:deviceId" $ do
    deviceId <- param "deviceId" :: ActionM T.Text
    json $ Device {deviceTypeId=10, macAddr="Impedence Measurement Device"}

  get "/api/1/devices/:deviceId/readings" $ do
    deviceId <- param "deviceId" :: ActionM T.Text
    json $ [Reading {value="1234"}, Reading{value="4321"}]

  post "/api/1/devices/:deviceId/readings" $ do
    deviceId <- param "deviceId" :: ActionM T.Text
    readingStr <- body
    let reading = decode readingStr :: Maybe Reading
    json $ case reading of
            Just r  -> Map.singleton "status" "ok" :: Map.Map T.Text T.Text
            Nothing -> Map.singleton "status" "wrong reading parameters" :: Map.Map T.Text T.Text
