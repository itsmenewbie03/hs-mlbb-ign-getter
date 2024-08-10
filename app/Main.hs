{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Data.Aeson
import Data.Text (Text, pack)
import qualified Data.Text.IO as T
import GHC.Generics
import Network.Wreq
import System.IO

data ReqBody = ReqBody
  { token :: Text
  , jmsId :: Text
  , appId :: Text
  , systemFlag :: Text
  , roleName :: Text
  , country :: Text
  , language :: Text
  , appAlias :: Text
  , platformName :: Text
  , serverId :: Text
  , goodsId :: Text
  , goodsSnapshotId :: Text
  , payTypeId :: Text
  , userId :: Text
  , activityId :: Text
  , serverName :: Text
  , domain :: Text
  , deviceId :: Text
  }
  deriving (Generic)

instance ToJSON ReqBody

data ResBody = ResBody
  { code :: Text
  , msg :: Text
  , data_my_ass :: UserData
  }
  deriving (Generic)

data UserData = UserData
  { isValid :: Int
  , nickName :: Text
  , isNewUser :: Int
  }
  deriving (Generic)

instance FromJSON ResBody where
  parseJSON = withObject "ResBody" $ \v -> do
    c <- v .: "code"
    m <- v .: "msg"
    d <- v .: "data"
    return $ ResBody c m d
instance FromJSON UserData

getBody :: Text -> Text -> ReqBody
getBody user_id zone_id =
  ReqBody
    { token = "6713293a41034f0facac787ba65c0aca"
    , jmsId = ""
    , appId = "APP20210608084718702"
    , systemFlag = "1.0"
    , roleName = ""
    , country = "ph"
    , language = "en"
    , appAlias = "mlbb_diamonds"
    , platformName = ""
    , serverId = zone_id
    , goodsId = "G20230807113729602"
    , goodsSnapshotId = ""
    , payTypeId = "983607"
    , userId = user_id
    , activityId = ""
    , serverName = ""
    , domain = "www.jollymax.com"
    , deviceId = "9a09edd6c0a1464892f715fb7f82a194"
    }
getIgn :: String -> String -> IO Text
getIgn user_id zone_id = do
  let reqbody = getBody (pack user_id) (pack zone_id)
  let opts = defaults & header "Accept" .~ ["application/json"]
  resp <- asJSON =<< postWith opts "https://api.jollymax.com/jolly-gateway/topup/order/check-uid" (toJSON reqbody)
  pure $ nickName $ data_my_ass (resp ^. responseBody)

main :: IO ()
main = do
  putStr "Enter GameID: "
  hFlush stdout
  user_id <- getLine
  putStr "Enter ZoneID: "
  hFlush stdout
  zone_id <- getLine
  ign <- getIgn user_id zone_id
  T.putStrLn $ "IGN: " <> ign
