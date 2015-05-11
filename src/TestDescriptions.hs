{-# LANGUAGE DeriveGeneric #-}

module TestDescriptions (
  Env(..),
  AddressState'(..),
  Exec(..),
  Transaction'(..),
--  CallCreate(..),
  RawData(..),
  InputWrapper(..),
  TestData(..),
  Tests
  ) where

-- External data modules
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Time.Clock (UTCTime(..))
import GHC.Generics (Generic)
import Network.Haskoin.Crypto (PrvKey(..))

-- Blockchain modules
import Blockchain.Data.Address (Address(..))
import Blockchain.Data.Code (Code(..))
import Blockchain.Data.Log (Log(..))
import Blockchain.SHA (SHA(..))
import Blockchain.VM.VMState (DebugCallCreate(..))

data Env =
  Env {
    currentCoinbase::Address,
    currentDifficulty::String,
    currentGasLimit::Integer,
    currentNumber::String,
    --currentTimestamp::String,
    currentTimestamp::UTCTime,
    previousHash::SHA
    } deriving (Generic, Show)

data AddressState' =
  AddressState' {
    nonce'::Integer,
    balance'::Integer,
    storage'::Map Integer Integer,
    contractCode'::Code
    } deriving (Generic, Show, Eq)

newtype RawData = RawData { theData::ByteString } deriving (Show, Eq)

data Exec =
  Exec {
    address'::Address,
    caller::Address,
    code::Code,
    data'::RawData,
    gas::String,
    gasPrice'::String,
    origin::Address,
    value'::String
    } deriving (Generic, Show)

data Transaction' =
  Transaction' {
    tData'::RawData,
    tGasLimit'::String,
    tGasPrice'::String,
    tNonce'::String,
    tSecretKey'::PrvKey,
    tTo'::Maybe Address,
    tValue'::String
    } deriving (Show)

data InputWrapper = IExec Exec | ITransaction Transaction' deriving (Show)

{-
data CallCreate =
  CallCreate {
    ccData::String,
    ccDestination::String,
    ccGasLimit::String,
    ccValue::String
    } deriving (Show, Eq)
-}

{-
data Log =
  Log {
    logAddress::String,
    logBloom'::String,
    logData::String,
    logTopics::[String]
    } deriving (Show, Eq)
-}

data TestData =
  Test {
    callcreates::Maybe [DebugCallCreate],
    env::Env,
    theInput::InputWrapper,
    {-
    exec::Maybe Exec,
    transaction::Maybe Transaction,
    -}
    remainingGas::Maybe Integer,
    logs'::[Log],
    out::RawData,
    pre::Map Address AddressState',
    post::Map Address AddressState'
    } deriving (Generic, Show)

type Tests = Map String TestData




