{-# LANGUAGE DeriveGeneric, OverloadedStrings, FlexibleInstances #-}

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Control.Monad.Trans.State
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Char
import qualified Data.HashMap.Lazy as H
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Scientific
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Clock.POSIX
import GHC.Generics hiding (to)
import qualified Network.Haskoin.Internals as Haskoin
import Network.Haskoin.Crypto (withSource)
import Numeric
import System.Environment
import System.Directory
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Blockchain.BlockChain
import qualified Blockchain.Colors as C
import Blockchain.Context
import Blockchain.Data.Address
import Blockchain.Data.AddressState
import Blockchain.Data.Block
import Blockchain.Data.Log
import Blockchain.Data.RLP
import Blockchain.Data.SignedTransaction
import Blockchain.Data.Transaction
import Blockchain.Database.MerklePatricia
import Blockchain.DB.CodeDB
import Blockchain.ExtDBs
import Blockchain.Format
import Blockchain.SHA
import Blockchain.Util
import Blockchain.VM
import Blockchain.VM.Code
import Blockchain.VM.Environment
import Blockchain.VM.VMState
import qualified Data.NibbleString as N

import Debug.Trace

import TestFiles

--debug = True
debug = False


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

type Storage = M.Map String String

data AddressState' =
  AddressState' {
    nonce'::Integer,
    balance'::Integer,
    storage'::M.Map String String,
    contractCode'::Code
    } deriving (Generic, Show, Eq)

newtype RawData = RawData { theData::B.ByteString } deriving (Show, Eq)

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
    tSecretKey'::Haskoin.PrvKey,
    tTo'::Maybe Address,
    tValue'::String
    } deriving (Show)

data InputWrapper = IExec Exec | ITransaction Transaction' deriving (Show)

data CallCreate =
  CallCreate {
    ccData::String,
    ccDestination::String,
    ccGasLimit::String,
    ccValue::String
    } deriving (Show)

{-
data Log =
  Log {
    logAddress::String,
    logBloom'::String,
    logData::String,
    logTopics::[String]
    } deriving (Show, Eq)
-}

data Test =
  Test {
    callcreates::Maybe [CallCreate],
    env::Env,
    theInput::InputWrapper,
    {-
    exec::Maybe Exec,
    transaction::Maybe Transaction,
    -}
    remainingGas::Maybe String,
    logs'::[Log],
    out::RawData,
    pre::M.Map String AddressState',
    post::M.Map String AddressState'
    } deriving (Generic, Show)

type Tests = M.Map String Test

instance FromJSON Test where
  parseJSON (Object v) | H.member "exec" v =
    test <$>
    v .:? "callcreates" <*>
    v .: "env" <*>
    v .: "exec" <*>
{-    v .: "exec" <*>
    v .: "transaction" <*> -}
    v .:? "gas" <*>
    v .:? "logs" .!= [] <*>
    v .:? "out" .!= (RawData B.empty) <*>
    v .: "pre" <*>
    v .:? "post" .!= M.empty
    where
       test v1 v2 exec v4 v5 v6 v7 v8 = Test v1 v2 (IExec exec) v4 v5 v6 v7 v8
  parseJSON (Object v) | H.member "transaction" v =
    test <$>
    v .:? "callcreates" <*>
    v .: "env" <*>
    v .: "transaction" <*>
{-    v .: "exec" <*>
    v .: "transaction" <*> -}
    v .:? "gas" <*>
    v .:? "logs" .!= [] <*>
    v .:? "out" .!= (RawData B.empty) <*>
    v .: "pre" <*>
    v .:? "post" .!= M.empty
    where
       test v1 v2 transaction v4 v5 v6 v7 v8 = Test v1 v2 (ITransaction transaction) v4 v5 v6 v7 v8
  parseJSON x = error $ "Missing case in parseJSON for Test: " ++ show x


{-
str2Int::Value->Parser Integer
str2Int (String s) =
  Parser {} -- read $ T.unpack s

qqqq :: Object -> Text -> Parser Integer
qqqq obj key = (
  case H.lookup key obj of
    Nothing -> fail $ "key " ++ show key ++ " not present"
    Just v  -> (str2Int v))::Parser Integer
  -}

--Same as an Integer, but can be pulled from json files as either a json number or string (like "2")
newtype SloppyInteger = SloppyInteger Integer

sloppyInteger2Integer::SloppyInteger->Integer
sloppyInteger2Integer (SloppyInteger x) = x

instance FromJSON SloppyInteger where
  parseJSON (Number x) = return $ SloppyInteger $ floor $ toRealFloat x
  parseJSON (String x) = return $ SloppyInteger $ floor $ toRealFloat $ read $ T.unpack x

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

instance FromJSON (Maybe Address) where
  parseJSON (String "") = pure Nothing
  parseJSON (String v) = fmap Just $ parseJSON (String v)

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

instance FromJSON Env where
  parseJSON (Object v) =
    env <$>
    v .: "currentCoinbase" <*>
    v .: "currentDifficulty" <*>
    v .: "currentGasLimit" <*>
    v .: "currentNumber" <*>
    v .: "currentTimestamp" <*>
    v .: "previousHash"
    where
      env v1 v2 currentGasLimit' v4 currentTimestamp' v6 =
        Env v1 v2 (read currentGasLimit') v4 (posixSecondsToUTCTime . fromInteger . sloppyInteger2Integer $ currentTimestamp') v6
               

instance FromJSON AddressState where
  parseJSON (Object v) =
    addressState <$>
    v .: "nonce" <*>
    v .: "balance" <*>
    v .: "storage" <*>
    v .: "code"
    where
      addressState::String->String->Object->SHA->AddressState
      addressState w x y z = AddressState (read w) (read x) emptyTriePtr z

instance FromJSON AddressState' where
  parseJSON (Object v) =
    addressState' <$>
    v .: "nonce" <*>
    v .: "balance" <*>
    v .: "storage" <*>
    v .: "code"
    where
      addressState'::String->String->M.Map String String->Code->AddressState'
      addressState' w x y z = AddressState' (read w) (read x) y z

instance FromJSON CallCreate where
  parseJSON (Object v) =
    CallCreate <$>
    v .: "data" <*>
    v .: "destination" <*>
    v .: "gasLimit" <*>
    v .: "value"

instance FromJSON Log where
  parseJSON (Object v) =
    log <$>
    v .: "address" <*>
    v .: "bloom" <*>
    v .: "data" <*>
    v .: "topics"
    where
      log v1 v2 v3 v4 = Log v1 (fromIntegral $ byteString2Integer $ fst $ B16.decode v2) v3 v4

instance FromJSON Address where
  parseJSON =
    withText "Address" $
    pure . Address . fromIntegral . byteString2Integer . fst . B16.decode . BC.pack . T.unpack

instance FromJSON B.ByteString where
  parseJSON =
    withText "Address" $
    pure . string2ByteString . T.unpack
    where
      string2ByteString::String->B.ByteString
      string2ByteString ('0':'x':rest) = fst . B16.decode . BC.pack $ rest
      string2ByteString x = fst . B16.decode . BC.pack $ x

instance FromJSON Code where
  parseJSON =
    withText "SHA" $
    pure . string2Code . T.unpack
    where
      string2Code::String->Code
      string2Code ('0':'x':rest) = bytes2Code . fst . B16.decode . BC.pack $ rest

instance FromJSON Haskoin.PrvKey where
  parseJSON =
    withText "PrvKey" $
    pure . Haskoin.PrvKey . fromInteger . byteString2Integer . fst . B16.decode . BC.pack . T.unpack

instance FromJSON RawData where
  parseJSON =
    withText "RawData" $
    pure . string2RawData . T.unpack
    where
      string2RawData::String->RawData
      string2RawData ('0':'x':rest) = RawData . fst . B16.decode . BC.pack $ rest
      string2RawData "" = RawData B.empty
      string2RawData x = error $ "Missing case in string2RawData: " ++ show x

instance FromJSON SHA where
  parseJSON =
    withText "SHA" $
    pure . string2SHA . T.unpack
    where
      string2SHA::String->SHA
      string2SHA ('0':'x':rest) = SHA . fromIntegral . byteString2Integer . fst . B16.decode . BC.pack $ rest
      string2SHA x = SHA . fromIntegral . byteString2Integer . fst . B16.decode . BC.pack $ x

instance FromJSON SHAPtr where
  parseJSON =
    withText "SHAPtr" $
    pure . SHAPtr . fst . B16.decode . BC.pack . T.unpack

{-
runCodeFromStart::Int->Integer->Environment->ContextM (VMState, SHAPtr)
runCodeFromStart callDepth' gasLimit' env = do
runCodeForTransaction'::Block->Int->Address->Address->Integer->Integer->Integer->Address->Code->B.ByteString->ContextM (B.ByteString, Integer)
runCodeForTransaction' b callDepth' sender origin value' gasPrice' availableGas owner code theData = do
data BlockData = BlockData {
  parentHash::SHA,
  unclesHash::SHA,
  coinbase::Address,
  bStateRoot::SHAPtr,
  transactionsRoot::SHAPtr,
  receiptsRoot::SHAPtr,
  logBloom::B.ByteString,
  difficulty::Integer,
  number::Integer,
  gasLimit::Integer,
  gasUsed::Integer,
  timestamp::UTCTime,
  extraData::Integer,
  nonce::SHA
} deriving (Show)

data Block = Block {
  blockData::BlockData,
  receiptTransactions::[SignedTransaction],
  blockUncles::[BlockData]
  } deriving (Show)
-}

showHexU::Integer->[Char]
showHexU = map toUpper . flip showHex ""

nibbleString2ByteString::N.NibbleString->B.ByteString
nibbleString2ByteString (N.EvenNibbleString s) = s
nibbleString2ByteString (N.OddNibbleString c s) = c `B.cons` s

hexString2Word256::String->Haskoin.Word256
hexString2Word256 "0x" = 0
hexString2Word256 ('0':'x':rest) =
  let
    [(value, "")] = readHex rest
  in
   fromIntegral value


populateAndConvertAddressState::AddressState'->ContextM AddressState
populateAndConvertAddressState addressState' = do
  addCode . codeBytes . contractCode' $ addressState'

  oldStorageStateRoot <- getStorageStateRoot
  setStorageStateRoot emptyTriePtr

  forM (M.toList $ storage' addressState') $ \(k, v) -> do
    putStorageKeyVal (hexString2Word256 k) (hexString2Word256 v)

  storageStateRoot <- getStorageStateRoot

  setStorageStateRoot oldStorageStateRoot

  return $
    AddressState
      (nonce' addressState')
      (balance' addressState')
      storageStateRoot 
      (hash $ codeBytes $ contractCode' addressState')





showHexInt::Integer->String
showHexInt 0 = "0x"
showHexInt x | odd $ length $ showHex x "" = "0x0" ++ showHex x ""
showHexInt x = "0x" ++ showHex x ""

getDataAndRevertAddressState::AddressState->ContextM AddressState'
getDataAndRevertAddressState addressState = do
  theCode <- fmap (fromMaybe (error $ "Missing code in getDataAndRevertAddressState: " ++ format addressState)) $
             getCode (codeHash addressState)
  sr <- getStorageStateRoot
  setStorageStateRoot (contractRoot addressState)
  storage <- getStorageKeyVals ""
  setStorageStateRoot sr
  return $
    AddressState'
    (addressStateNonce addressState)
    (balance addressState)
    (M.fromList (map (\(k, v) -> (showHexInt $ byteString2Integer $ nibbleString2ByteString k, showHexInt $ rlpDecode $ rlpDeserialize $ rlpDecode v)) storage))
    (bytes2Code theCode)

formatAddressState::AddressState'->String
formatAddressState = show

getNumber::String->Integer
getNumber "" = 0
getNumber x = read x

runTest::Test->ContextM (Either String String)
runTest test = do
  setStateRoot emptyTriePtr
  setStorageStateRoot emptyTriePtr

  --liftIO . putStrLn . show . hash . codeBytes . code . exec $ test

  forM (M.toList $ pre test) $ \(address, addressState') -> do
    addressState <- populateAndConvertAddressState addressState'
    --liftIO $ putStrLn $ show addressState
    putAddressState (Address . fromIntegral . byteString2Integer . fst . B16.decode . BC.pack $ address) addressState


  allAddressStates <- getAllAddressStates
  allAddressStates' <-
      forM allAddressStates $ \(k, a') -> do
        --liftIO $ putStrLn $ "-------\n" ++ show k ++ show a'
        a <- getDataAndRevertAddressState a'
        return (BC.unpack $ B16.encode $ nibbleString2ByteString k, a)


  when debug $ do
    liftIO $ putStrLn $ show (pre test)
    liftIO $ putStrLn "allAddressStates'-------------"
    liftIO $ putStrLn $ show $ M.fromList allAddressStates'

  let block =
        Block {
          blockData = BlockData {
             parentHash = previousHash . env $ test,
             number = read . currentNumber . env $ test,
             coinbase = currentCoinbase . env $ test,
             difficulty = read . currentDifficulty . env $ test,
             unclesHash = error "unclesHash undefined",
             bStateRoot = error "bStateRoot undefined",
             transactionsRoot = error "transactionsRoot undefined",
             receiptsRoot = error "receiptsRoot undefined",
             logBloom = error "logBloom undefined",
             gasLimit = currentGasLimit . env $ test,
             gasUsed = error "gasUsed undefined",
             timestamp = currentTimestamp . env $ test,
             --timestamp = posixSecondsToUTCTime . fromInteger . read . currentTimestamp . env $ test,
             extraData = error "extraData undefined",
             nonce = error "nonce undefined"
             },
          receiptTransactions = error "receiptTransactions undefined",
          blockUncles = error "blockUncles undefined"
          }


  --runCodeFromStart

  -- (result, remainingGas', maybeException) <-
  newVMState <-
    case theInput test of
      IExec exec ->

        runCodeFromStart 0 (getNumber $ gas exec)
          Environment{
            envGasPrice=getNumber . gasPrice' $ exec,
            envBlock=block,
            envOwner = address' exec,
            envOrigin = origin exec,
            envInputData = theData . data' $ exec,
            envSender = caller exec,
            envValue = getNumber . value' $ exec,
            envCode = code exec
            }

      ITransaction transaction -> do
        let unsignedTransaction =
              case tTo' transaction of
                Nothing ->
                  ContractCreationTX {
                    tNonce = getNumber $ tNonce' transaction,
                    gasPrice = getNumber $ tGasPrice' transaction,
                    tGasLimit = getNumber $ tGasLimit' transaction,
                    value = getNumber $ tValue' transaction,
                    tInit = bytes2Code $ theData $ tData' transaction
                    }
                Just a ->
                  MessageTX {
                    tNonce = getNumber $ tNonce' transaction,
                    gasPrice = getNumber $ tGasPrice' transaction,
                    tGasLimit = getNumber $ tGasLimit' transaction,
                    to = a,
                    value = getNumber $ tValue' transaction,
                    tData = theData $ tData' transaction
                    }

        signedTransaction <-
          liftIO $ withSource Haskoin.devURandom $ 
          signTransaction
          (tSecretKey' transaction)
          unsignedTransaction
        fmap fst $ addTransaction block (currentGasLimit $ env test) signedTransaction




{-
  liftIO $ print result
  liftIO $ print (out test)
  liftIO $ putStrLn $ "    STORAGE"
  kvs <- getStorageKeyVals ""
  liftIO $ putStrLn $ unlines (map (\(k, v) -> "0x" ++ showHexU (byteString2Integer $ nibbleString2ByteString k) ++ ": 0x" ++ showHexU (rlpDecode $ rlpDeserialize $ rlpDecode v::Integer)) kvs)
  liftIO $ print (post test)
-}

  allAddressStates <- getAllAddressStates
  allAddressStates' <-
      forM allAddressStates $ \(k, a') -> do
        when debug $ liftIO $ putStrLn $ "-------\n" ++ show (pretty k) ++ format a'
        a <- getDataAndRevertAddressState a'
        return (BC.unpack $ B16.encode $ nibbleString2ByteString k, a)


  when debug $ do
    liftIO $ putStrLn $ show (pre test)
    liftIO $ putStrLn "allAddressStates'-------------"
    liftIO $ putStrLn $ intercalate "\n" $ (\(k, v) -> C.yellow k ++ ": " ++ formatAddressState v) <$> allAddressStates'
    liftIO $ putStrLn "post test-------------"
    liftIO $ putStrLn $ intercalate "\n" $ (\(k, v) -> C.yellow k ++ ": " ++ formatAddressState v) <$> (M.toList $ post test)
    liftIO $ putStrLn "-------------"
    liftIO $ putStrLn $ "result Log: " ++ show (logs newVMState)
    liftIO $ putStrLn "-------------"
    liftIO $ putStrLn $ "expected Log: " ++ show (logs' test)
    liftIO $ putStrLn "-------------"

{-  case (RawData result == out test,
        (M.fromList allAddressStates' == post test) || (M.null (post test) && isJust maybeException),
        case remainingGas test of
          Nothing -> True
          Just x -> remainingGas' == read x,
        [] == logs test) of-}
  case (True,
        (M.fromList allAddressStates' == post test) || (M.null (post test) && isJust (vmException newVMState)),
        case remainingGas test of
          Nothing -> True
          Just x -> vmGasRemaining newVMState == read x,
        logs newVMState == reverse (logs' test)) of
    (False, _, _, _) -> return $ Left "result doesn't match"
    (_, False, _, _) -> return $ Left "address states don't match"
    (_, _, False, _) -> return $ Left $ "remaining gas doesn't match: is " ++ show (vmGasRemaining newVMState) ++ ", should be " ++ show (remainingGas test)
    (_, _, _, False) -> do
      liftIO $ putStrLn "llllllllllllllllllllll"
      liftIO $ putStrLn $ show $ logs newVMState
      liftIO $ putStrLn "llllllllllllllllllllll"
      liftIO $ putStrLn $ show $ logs' test
      liftIO $ putStrLn "llllllllllllllllllllll"
      return $ Left "logs don't match"
    _ -> return $ Right "Success"

formatResult::(String, Either String String)->String
formatResult (name, Left err) = "> " ++ name ++ ": " ++ C.red err
formatResult (name, Right message) = "> " ++ name ++ ": " ++ C.green message

runTests::[(String, Test)]->ContextM ()
runTests tests = do
  results <- 
    forM tests $ \(name, test) -> do
      --qqqqliftIO $ putStrLn $ "Running test: " ++ show name
      result <- runTest test
      return (name, result)
  liftIO $ putStrLn $ intercalate "\n" $ formatResult <$> results

main = do
  testsExist <- doesDirectoryExist "tests"
  when (not testsExist) $
    error "You need to clone the git repository at https://github.com/ethereum/tests.git"

  args <- getArgs

  let (maybeFileName, maybeTestName) = 
        case args of
          [] -> (Nothing, Nothing)
          [x] -> (Just x, Nothing)
          [x, y] -> (Just x, Just y)

  --let theFileName = testFiles !! read fileNumber

  runResourceT $ do
    cxt <- openDBs "h"
    liftIO $ runStateT (runAllTests maybeFileName maybeTestName) cxt


runAllTests::Maybe String->Maybe String->ContextM ()
runAllTests maybeFileName maybeTestName= do
  let theFiles =
        case maybeFileName of
          Nothing -> testFiles
          Just fileName -> [fileName]
    
  forM_ theFiles $ \theFileName -> do
      theFile <- liftIO $ BL.readFile theFileName
      liftIO $ putStrLn $ C.yellow $ "#### Running tests in file: " ++ theFileName
      runTestsInFile maybeTestName theFile

runTestsInFile::Maybe String->BL.ByteString->ContextM ()
runTestsInFile maybeTestName theFile = do

  case fmap fromJSON $ eitherDecode theFile::Either String (Result Tests) of
    Left err -> liftIO $ putStrLn err
    Right val ->
      case val of
        Error err' -> liftIO $ putStrLn err'
        Success tests -> runTests (filter ((matchName maybeTestName) . fst) (M.toList tests))
  where
    matchName::Maybe String->String->Bool
    matchName Nothing _ = True
    matchName (Just x1) x2 | x1 == x2 = True
    matchName _ _ = False
