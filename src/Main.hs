{-# LANGUAGE DeriveGeneric, OverloadedStrings, FlexibleInstances #-}

import Control.Applicative
import Control.Monad
import Control.Monad.IfElse
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Control.Monad.Trans.Resource
import Control.Monad.Trans.State
import qualified Crypto.Hash.SHA3 as SHA3
import Data.Aeson
import qualified Data.Binary as Bin
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Either
import Data.List
import qualified Data.Map as M
import Data.Maybe
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
import Blockchain.Data.AddressStateDB
import Blockchain.Data.BlockDB
import Blockchain.Data.Code
import Blockchain.Context
import Blockchain.Data.DataDefs
import Blockchain.Data.RLP
import Blockchain.Data.Transaction
import Blockchain.Database.MerklePatricia
import Blockchain.DB.CodeDB
import Blockchain.DBM
import Blockchain.ExtDBs
import Blockchain.ExtWord
import Blockchain.Format
import Blockchain.SHA
import Blockchain.Util
import Blockchain.VM
import Blockchain.VM.Code
import Blockchain.VM.Environment
import Blockchain.VM.VMState
import qualified Data.NibbleString as N

import TestDescriptions

import Debug.Trace

import TestFiles

nibbleString2ByteString::N.NibbleString->B.ByteString
nibbleString2ByteString (N.EvenNibbleString str) = str
nibbleString2ByteString (N.OddNibbleString c str) = c `B.cons` str


populateAndConvertAddressState::Address->AddressState'->ContextM AddressState
populateAndConvertAddressState owner addressState' = do
  lift . addCode . codeBytes . contractCode' $ addressState'

  forM_ (M.toList $ storage' addressState') $
    \(key, val) -> do putStorageKeyVal' owner (fromIntegral key) (fromIntegral val)

  addressState <- lift $ getAddressState owner

  return $
    AddressState
      (nonce' addressState')
      (balance' addressState')
      (addressStateContractRoot addressState)
      (hash $ codeBytes $ contractCode' addressState')





showHexInt::Integer->String
showHexInt x
  = let xHex = showHex x ""
    in (if odd $ length xHex
        then "0x0"
        else "0x")
       ++ xHex

getDataAndRevertAddressState::Address->AddressState->ContextM AddressState'
getDataAndRevertAddressState owner addressState = do
  theCode <- lift $ fmap (fromMaybe (error $ "Missing code in getDataAndRevertAddressState: " ++ format addressState)) $
             getCode (addressStateCodeHash addressState)
  storage <- getAllStorageKeyVals' owner
  return $
    AddressState'
    (addressStateNonce addressState)
    (addressStateBalance addressState)
    (M.mapKeys (byteString2Integer . nibbleString2ByteString)
     . M.map (fromIntegral)
     $ M.fromList storage)
    (Code theCode)

formatAddressState::AddressState'->String
formatAddressState = show

getNumber::String->Integer
getNumber "" = 0
getNumber x = read x

{-
newAccountsToCallCreates::(Maybe Address, Integer, AddressState)->ContextM DebugCallCreate
newAccountsToCallCreates (maybeAddress, gasRemaining, AddressState{balance=b, codeHash=h}) = do
  Just codeBytes <- lift $ getCode h
  let destination =
        case maybeAddress of
          Just (Address address) -> padZeros 40 $ showHex address ""
          Nothing -> ""
  return DebugCallCreate {
    ccData="0x" ++ BC.unpack (B16.encode codeBytes),
    ccDestination=destination,
    ccGasLimit=show gasRemaining,
    ccValue=show b
    }
-}

isBlankCode::Code->Bool
isBlankCode (Code "") = True
isBlankCode _ = False


showInfo::(Address,AddressState')->String
showInfo (key,val@AddressState'{nonce'=n, balance'=b, storage'=s, contractCode'=Code c}) = 
    show (pretty key) ++ "(" ++ show n ++ "): " ++ show b ++ 
         (if M.null s then "" else ", " ++ show (M.toList $ M.map showHexInt $ M.mapKeys showHexInt $ s)) ++ 
         (if B.null c then "" else ", CODE:[" ++ C.blue (format c) ++ "]")


runTest::Test->ContextM (Either String String)
runTest test = do
  lift $ setStateRoot emptyTriePtr

  forM_ (M.toList $ pre test) $
    \(addr, s) -> do
      state <- populateAndConvertAddressState addr s
      lift $ putAddressState addr state

  beforeAddressStates <- do
    let testAddrs = M.keys $ pre test
    beforeStates <- lift $ mapM getAddressState testAddrs
    beforeStates' <- mapM (uncurry getDataAndRevertAddressState)
                    $ zip testAddrs beforeStates
    return $ zip testAddrs beforeStates'

  let block =
        Block {
          blockBlockData = BlockData {
             blockDataParentHash = previousHash . env $ test,
             blockDataNumber = read . currentNumber . env $ test,
             blockDataCoinbase = currentCoinbase . env $ test,
             blockDataDifficulty = read . currentDifficulty . env $ test,
             blockDataUnclesHash = error "unclesHash undefined",
             blockDataStateRoot = error "bStateRoot undefined",
             blockDataTransactionsRoot = error "transactionsRoot undefined",
             blockDataReceiptsRoot = error "receiptsRoot undefined",
             blockDataLogBloom = error "logBloom undefined",
             blockDataGasLimit = currentGasLimit . env $ test,
             blockDataGasUsed = error "gasUsed undefined",
             blockDataTimestamp = currentTimestamp . env $ test,
             --timestamp = posixSecondsToUTCTime . fromInteger . read . currentTimestamp . env $ test,
             blockDataExtraData = error "extraData undefined",
             blockDataNonce = error "nonce undefined"
             },
          blockReceiptTransactions = error "receiptTransactions undefined",
          blockBlockUncles = error "blockUncles undefined"
          }

  (result, retVal, gasRemaining, logs, returnedCallCreates) <-
    case theInput test of
      IExec exec -> do

        let env =
              Environment{
                envGasPrice=getNumber $ gasPrice' exec,
                envBlock=block,
                envOwner = address' exec,
                envOrigin = origin exec,
                envInputData = theData $ data' exec,
                envSender = caller exec,
                envValue = getNumber $ value' exec,
                envCode = code exec,
                envJumpDests = getValidJUMPDESTs $ code exec
                }

        vmState <- liftIO $ startingState env

        (result, vmState) <-
          flip runStateT vmState{vmGasRemaining=getNumber $ gas exec, debugCallCreates=Just []} $
          runEitherT $ do
            runCodeFromStart

            vmState <- lift get
            whenM (lift $ lift isDebugEnabled) $ do
              liftIO $ putStrLn $ "Removing accounts in suicideList: " ++
                                intercalate ", " (show . pretty <$> suicideList vmState)

            forM_ (suicideList vmState) $ lift . lift . lift . deleteAddressState

        return (result, returnVal vmState, vmGasRemaining vmState, logs vmState, debugCallCreates vmState)

      ITransaction transaction -> do
        let t = case tTo' transaction of
                Nothing ->
                  createContractCreationTX
                    (getNumber $ tNonce' transaction)
                    (getNumber $ tGasPrice' transaction)
                    (getNumber $ tGasLimit' transaction)
                    (getNumber $ tValue' transaction)
                    (Code $ theData $ tData' transaction)
                    (tSecretKey' transaction)
                Just a ->
                  createMessageTX
                    (getNumber $ tNonce' transaction)
                    (getNumber $ tGasPrice' transaction)
                    (getNumber $ tGasLimit' transaction)
                    a
                    (getNumber $ tValue' transaction)
                    (theData $ tData' transaction)
                    (tSecretKey' transaction)
        signedTransaction <- liftIO $ withSource Haskoin.devURandom t
        result <-
          runEitherT $ addTransaction block (currentGasLimit $ env test) signedTransaction

        case result of
          Right (vmState, _) ->
            return (Right (), returnVal vmState, vmGasRemaining vmState, logs vmState, debugCallCreates vmState)
          Left e -> return (Right (), Nothing, 0, [], Just [])

  afterAddressStates <- do
    let testAddrs = M.keys (pre test)
    afterStates <- lift $ mapM getAddressState testAddrs
    afterStates' <- mapM (uncurry getDataAndRevertAddressState)
                    $ zip testAddrs afterStates
    return $ zip testAddrs afterStates'

  let hashInteger = byteString2Integer . nibbleString2ByteString . N.EvenNibbleString . (SHA3.hash 256) . nibbleString2ByteString . N.pack . (N.byte2Nibbles =<<) . word256ToBytes . fromIntegral

  let postTest = M.toList $
        flip M.map (post test) $
        \s' -> s'{storage' = M.mapKeys hashInteger (storage' s')} 

  whenM isDebugEnabled $ do
    liftIO $ putStrLn "Before-------------"
    liftIO $ putStrLn $ unlines $ showInfo <$> beforeAddressStates
    liftIO $ putStrLn "After-------------"
    liftIO $ putStrLn $ unlines $ showInfo <$> afterAddressStates
    liftIO $ putStrLn "Expected-------------"
    liftIO $ putStrLn $ unlines $ showInfo <$> postTest
    liftIO $ putStrLn "-------------"

  case (RawData (fromMaybe B.empty retVal) == out test,
        (afterAddressStates == postTest) || (null postTest && isLeft result),
        case remainingGas test of
          Nothing -> True
          Just x -> gasRemaining == x,
        logs == reverse (logs' test),
        (callcreates test == fmap reverse returnedCallCreates) || (isNothing (callcreates test) && (returnedCallCreates == Just []))
        ) of
    (False, _, _, _, _) -> return $ Left "result doesn't match"
    (_, False, _, _, _) -> return $ Left "address states don't match"
    (_, _, False, _, _) -> return $ Left $ "remaining gas doesn't match: is " ++ show gasRemaining ++ ", should be " ++ show (remainingGas test) ++ ", diff=" ++ show (gasRemaining - fromJust (remainingGas test))
    (_, _, _, False, _) -> do
      liftIO $ putStrLn "llllllllllllllllllllll"
      liftIO $ putStrLn $ show $ logs
      liftIO $ putStrLn "llllllllllllllllllllll"
      liftIO $ putStrLn $ show $ logs' test
      liftIO $ putStrLn "llllllllllllllllllllll"
      return $ Left "logs don't match"
    (_, _, _, _, False) -> do
      liftIO $ do
        putStrLn $ "callcreates test = " ++ show (callcreates test)
        putStrLn $ "returnedCallCreates = " ++ show returnedCallCreates
      
      return $ Left $ "callcreates don't match"
    _ -> return $ Right "Success"

formatResult::(String, Either String String)->String
formatResult (name, Left err) = "> " ++ name ++ ": " ++ C.red err
formatResult (name, Right message) = "> " ++ name ++ ": " ++ C.green message

runTests::[(String, Test)]->ContextM ()
runTests tests = do
  results <- 
    forM tests $ \(name, test) -> do
      --liftIO $ putStrLn $ "Running test: " ++ show name
      result <- runTest test
      return (name, result)
  liftIO $ putStrLn $ intercalate "\n" $ formatResult <$> results

main::IO ()
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
          _ -> error "You can only supply 2 parameters"
  
  --let theFileName = testFiles !! read fileNumber


  _ <- runResourceT $ do
    cxt <- openDBs "h"
    let debug = length args == 2
    runStateT (runStateT (runAllTests maybeFileName maybeTestName) (Context [] 0 [] debug)) cxt

  return ()

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
