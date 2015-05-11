{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module JSON (load) where

-- External data modules
import Control.Applicative (pure, (<*>))
import Data.Aeson

-- External function modules
import qualified Data.ByteString as ByteS
import qualified Data.ByteString.Lazy as ByteSL (readFile)
import qualified Data.ByteString.Base16 as ByteS16
import qualified Data.ByteString.Char8 as ByteS8
import Data.ByteString.Lazy (readFile)
import Data.Functor ((<$>))
import Data.HashMap.Lazy (member)
import qualified Data.Map as Map
import Data.Maybe (isNothing, fromJust)
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Network.Haskoin.Crypto (PrvKey(..))
import Numeric (readHex)

-- Blockchain modules
import Blockchain.Data.Address (Address(..))
import Blockchain.Data.Code (Code(..))
import Blockchain.Data.Log (Log(..))
import Blockchain.Database.MerklePatricia (SHAPtr(..))
import Blockchain.Util (byteString2Integer)
import Blockchain.SHA (SHA(..))
import Blockchain.VM.VMState (DebugCallCreate(..))

-- Internal modules
import TestDescriptions

load::String -> IO Tests
load fileName =  do
  fileContents <- ByteSL.readFile fileName 
  let tests = decode fileContents
  return $
    if isNothing tests
    then Map.empty
    else fromJust tests


convertAddressAndAddressInfo = Map.fromList . map convertPre' . Map.toList
  where
    convertPre' (addressString, addressState) =
      (Address $ fromInteger $ byteString2Integer $ fst $
       ByteS16.decode $ ByteS8.pack addressString, addressState)

instance FromJSON TestData where
  parseJSON (Object v) | "exec" `member` v =
    test <$>
    v .:? "callcreates" <*>
    v .: "env" <*>
    v .: "exec" <*>
{-    v .: "exec" <*>
    v .: "transaction" <*> -}
    v .:? "gas" <*>
    v .:? "logs" .!= [] <*>
    v .:? "out" .!= (RawData ByteS.empty) <*>
    v .: "pre" <*>
    v .:? "post" .!= Map.empty
    where
       test v1 v2 exec gas' v5 v6 v7 v8 = Test v1 v2 (IExec exec)(fmap read gas')v5 v6 (convertAddressAndAddressInfo v7) (convertAddressAndAddressInfo v8)
       
  parseJSON (Object v) | "transaction" `member` v =
    test <$>
    v .:? "callcreates" <*>
    v .: "env" <*>
    v .: "transaction" <*>
{-    v .: "exec" <*>
    v .: "transaction" <*> -}
    v .:? "gas" <*>
    v .:? "logs" .!= [] <*>
    v .:? "out" .!= (RawData ByteS.empty) <*>
    v .: "pre" <*>
    v .:? "post" .!= Map.empty
    where
       test v1 v2 transaction gas' v5 v6 v7 v8 = Test v1 v2 (ITransaction transaction) (fmap read gas') v5 v6 (convertAddressAndAddressInfo v7) (convertAddressAndAddressInfo v8)
  parseJSON x = error $ "Missing case in parseJSON for Test: " ++ show x


--Same as an Integer, but can be pulled from json files as either a json number or string (like "2")
newtype SloppyInteger = SloppyInteger Integer

sloppyInteger2Integer::SloppyInteger->Integer
sloppyInteger2Integer (SloppyInteger x) = x

instance FromJSON SloppyInteger where
  parseJSON (Number x) = return $ SloppyInteger $ floor x
  parseJSON (String x) = return $ SloppyInteger $ floor $ (read $ Text.unpack x::Double)
  parseJSON x = error $ "Wrong format when trying to parse SloppyInteger from JSON: " ++ show x

instance FromJSON Exec where
  parseJSON (Object v) =
    Exec <$>
    v .: "address" <*>
    v .: "caller" <*>
    v .: "code" <*>
    v .: "data" <*>
    v .: "gas" <*>
    v .: "gasPrice" <*>
    v .: "origin" <*>
    v .: "value"
  parseJSON x = error $ "Wrong format when trying to parse Exec from JSON: " ++ show x

instance FromJSON (Maybe Address) where
  parseJSON (String "") = pure Nothing
  parseJSON (String v) = fmap Just $ parseJSON (String v)
  parseJSON x = error $ "Wrong format when trying to parse 'Maybe Address' from JSON: " ++ show x

instance FromJSON Transaction' where
  parseJSON (Object v) =
    Transaction' <$>
    v .: "data" <*>
    v .: "gasLimit" <*>
    v .: "gasPrice" <*>
    v .: "nonce" <*>
    v .: "secretKey" <*>
    v .: "to" <*>
    v .: "value"
  parseJSON x = error $ "Wrong format when trying to parse Transaction' from JSON: " ++ show x

instance FromJSON Env where
  parseJSON (Object v) =
    env' <$>
    v .: "currentCoinbase" <*>
    v .: "currentDifficulty" <*>
    v .: "currentGasLimit" <*>
    v .: "currentNumber" <*>
    v .: "currentTimestamp" <*>
    v .: "previousHash"
    where
      env' v1 v2 currentGasLimit' v4 currentTimestamp' v6 =
        Env v1 v2 (read currentGasLimit') v4 (posixSecondsToUTCTime . fromInteger . sloppyInteger2Integer $ currentTimestamp') v6
  parseJSON x = error $ "Wrong format when trying to parse Env from JSON: " ++ show x

instance FromJSON AddressState' where
  parseJSON (Object v) =
    addressState' <$>
    v .: "nonce" <*>
    v .: "balance" <*>
    v .: "storage" <*>
    v .: "code"
    where
      addressState'::String->String->Map.Map String String->Code->AddressState'
      addressState' w x y z = AddressState' (hexOrDecString2Integer w) (hexOrDecString2Integer x) (readMap y) z
      readMap = (Map.map hexOrDecString2Integer) . (Map.mapKeys hexOrDecString2Integer)
      hexOrDecString2Integer "0x" = 0
      hexOrDecString2Integer ('0':'x':rest) =
        let [(val, "")] = readHex rest
        in val
      hexOrDecString2Integer x = read x
  parseJSON x = error $ "Wrong format when trying to parse AddressState' from JSON: " ++ show x

instance FromJSON DebugCallCreate where
  parseJSON (Object v) =
    debugCallCreate' <$>
    v .: "data" <*>
    v .: "destination" <*>
    v .: "gasLimit" <*>
    v .: "value"
    where
      debugCallCreate' v1 v2 gasLimit val = DebugCallCreate v1 v2 (read gasLimit) (read val)
  parseJSON x = error $ "Wrong format when trying to parse CallCreate from JSON: " ++ show x

instance FromJSON Log where
  parseJSON (Object v) =
    log' <$>
    v .: "address" <*>
    v .: "bloom" <*>
    v .: "data" <*>
    v .: "topics"
    where
      log' v1 v2 v3 v4 = Log v1 (fromIntegral $ byteString2Integer $ fst $ ByteS16.decode v2) v3 v4
  parseJSON x = error $ "Wrong format when trying to parse Log from JSON: " ++ show x

instance FromJSON Address where
  parseJSON =
    withText "Address" $
    pure . Address . fromIntegral . byteString2Integer . fst . b16_decode_optional0x . ByteS8.pack . Text.unpack
    where
      b16_decode_optional0x x = 
        case ByteS8.unpack x of
          ('0':'x':rest) -> ByteS16.decode $ ByteS8.pack rest
          _ -> ByteS16.decode x

instance FromJSON ByteS.ByteString where
  parseJSON =
    withText "Address" $
    pure . string2ByteString . Text.unpack
    where
      string2ByteString ('0':'x':rest) = fst . ByteS16.decode . ByteS8.pack $ rest
      string2ByteString x = fst . ByteS16.decode . ByteS8.pack $ x

instance FromJSON Code where
  parseJSON =
    withText "SHA" $
    pure . string2Code . Text.unpack
    where
      string2Code::String->Code
      string2Code ('0':'x':rest) = Code . fst . ByteS16.decode . ByteS8.pack $ rest
      string2Code x = error $ "string2Code called with input of wrong format: " ++ x

instance FromJSON PrvKey where
  parseJSON =
    withText "PrvKey" $
    pure . PrvKey . fromInteger . byteString2Integer . fst . ByteS16.decode . ByteS8.pack . Text.unpack

instance FromJSON RawData where
  parseJSON =
    withText "RawData" $
    pure . string2RawData . Text.unpack
    where
      string2RawData ('0':'x':rest) = RawData . fst . ByteS16.decode . ByteS8.pack $ rest
      string2RawData "" = RawData ByteS.empty
      string2RawData x = error $ "Missing case in string2RawData: " ++ show x

instance FromJSON SHA where
  parseJSON =
    withText "SHA" $
    pure . string2SHA . Text.unpack
    where
      string2SHA::String->SHA
      string2SHA ('0':'x':rest) = SHA . fromIntegral . byteString2Integer . fst . ByteS16.decode . ByteS8.pack $ rest
      string2SHA x = SHA . fromIntegral . byteString2Integer . fst . ByteS16.decode . ByteS8.pack $ x

instance FromJSON SHAPtr where
  parseJSON =
    withText "SHAPtr" $
    pure . SHAPtr . fst . ByteS16.decode . ByteS8.pack . Text.unpack
