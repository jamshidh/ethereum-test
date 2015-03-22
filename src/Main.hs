{-# LANGUAGE DeriveGeneric, OverloadedStrings, FlexibleInstances #-}

import Control.Applicative
import Control.Monad
import Control.Monad.IfElse
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Control.Monad.Trans.Resource
import Control.Monad.Trans.State
import Data.Aeson
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
import Blockchain.Data.AddressState
import Blockchain.Data.Block
import Blockchain.Data.Code
import Blockchain.Context
import Blockchain.Data.RLP
import Blockchain.Data.SignedTransaction
import Blockchain.Data.Transaction
import Blockchain.Database.MerklePatricia
import Blockchain.DB.CodeDB
import Blockchain.DBM
import Blockchain.ExtDBs
import Blockchain.Format
import Blockchain.SHA
import Blockchain.SigningTools
import Blockchain.Util
import Blockchain.VM
import Blockchain.VM.Code
import Blockchain.VM.Environment
import Blockchain.VM.VMState
import qualified Data.NibbleString as N

import TestDescriptions

--import Debug.Trace

import TestFiles

nibbleString2ByteString::N.NibbleString->B.ByteString
nibbleString2ByteString (N.EvenNibbleString str) = str
nibbleString2ByteString (N.OddNibbleString c str) = c `B.cons` str

hexString2Word256::String->Haskoin.Word256
hexString2Word256 "0x" = 0
hexString2Word256 ('0':'x':rest) =
  let
    [(val, "")] = readHex rest
  in
   val
hexString2Word256 x = error $ "hexString2Word256 called with input of wrong format: " ++ x



populateAndConvertAddressState::Address->AddressState'->ContextM AddressState
populateAndConvertAddressState owner addressState' = do
  lift . addCode . codeBytes . contractCode' $ addressState'

  forM_ (M.toList $ storage' addressState') $ \(key, val) -> do
    putStorageKeyVal' owner (hexString2Word256 key) (hexString2Word256 val)

  addressState <- lift $ getAddressState owner

  return $
    AddressState
      (nonce' addressState')
      (balance' addressState')
      (contractRoot addressState)
      (hash $ codeBytes $ contractCode' addressState')





showHexInt::Integer->String
showHexInt 0 = "0x"
showHexInt x | odd $ length $ showHex x "" = "0x0" ++ showHex x ""
showHexInt x = "0x" ++ showHex x ""

getDataAndRevertAddressState::Address->AddressState->ContextM AddressState'
getDataAndRevertAddressState owner addressState = do
  theCode <- lift $ fmap (fromMaybe (error $ "Missing code in getDataAndRevertAddressState: " ++ format addressState)) $
             getCode (codeHash addressState)
  storage <- getAllStorageKeyVals' owner
  return $
    AddressState'
    (addressStateNonce addressState)
    (balance addressState)
    (M.fromList (map (\(key, val) -> (showHexInt $ byteString2Integer $ nibbleString2ByteString key, showHexInt $ fromIntegral val)) storage))
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
         (if M.null s then "" else ", " ++ show (M.toList s)) ++ 
         (if B.null c then "" else ", CODE:[" ++ C.blue (format c) ++ "]")


runTest::Test->ContextM (Either String String)
runTest test = do
  lift $ setStateRoot emptyTriePtr

  forM_ (M.toList $ pre test) $ \(address, addressState') -> do
    addressState <- populateAndConvertAddressState address addressState'
    lift $ putAddressState address addressState


  allAddressStates <- lift getAllAddressStates
  allAddressStates' <-
      forM allAddressStates $ \(k, a') -> do
        a <- getDataAndRevertAddressState k a'
        return (k, a)

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


  (result, newVMState) <-
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

        
        flip runStateT vmState{vmGasRemaining=getNumber $ gas exec, debugCallCreates=Just []} $
          runEitherT $ do
            runCodeFromStart 0

            vmState <- lift get
            whenM (lift $ lift isDebugEnabled) $ do
              liftIO $ putStrLn $ "Removing accounts in suicideList: " ++
                                intercalate ", " (show . pretty <$> suicideList vmState)

            forM_ (suicideList vmState) $ lift . lift . lift . deleteAddressState

      

      ITransaction transaction -> do
        let ut =
              case tTo' transaction of
                Nothing ->
                  ContractCreationTX {
                    tNonce = getNumber $ tNonce' transaction,
                    gasPrice = getNumber $ tGasPrice' transaction,
                    tGasLimit = getNumber $ tGasLimit' transaction,
                    value = getNumber $ tValue' transaction,
                    tInit = Code $ theData $ tData' transaction
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
          ut
        (vmState, _) <- addTransaction block (currentGasLimit $ env test) signedTransaction

        return (Right (), vmState)

  allAddressStates2 <- lift getAllAddressStates
  allAddressStates3 <-
      forM allAddressStates2 $ \(k, a') -> do
        a <- getDataAndRevertAddressState k a'
        return (k, a)


  whenM isDebugEnabled $ do
    liftIO $ putStrLn "Before-------------"
    liftIO $ putStrLn $ unlines $ showInfo <$> allAddressStates'
    liftIO $ putStrLn "allAddressStates'-------------"
    liftIO $ putStrLn $ unlines $ showInfo <$> allAddressStates3
    liftIO $ putStrLn "post test-------------"
    liftIO $ putStrLn $ unlines $ showInfo <$> (M.toList $ post test)
    liftIO $ putStrLn "-------------"
    liftIO $ putStrLn $ "result Log: " ++ show (logs newVMState)
    liftIO $ putStrLn "-------------"
    liftIO $ putStrLn $ "expected Log: " ++ show (logs' test)
    liftIO $ putStrLn "-------------"

  case (RawData (fromMaybe B.empty $ returnVal newVMState) == out test,
        (M.fromList allAddressStates3 == post test) || (M.null (post test) && isLeft result),
        case remainingGas test of
          Nothing -> True
          Just x -> vmGasRemaining newVMState == x,
        logs newVMState == reverse (logs' test),
        (callcreates test == fmap reverse (debugCallCreates newVMState)) || (isNothing (callcreates test) && (debugCallCreates newVMState == Just []))
        ) of
    (False, _, _, _, _) -> return $ Left "result doesn't match"
    (_, False, _, _, _) -> return $ Left "address states don't match"
    (_, _, False, _, _) -> return $ Left $ "remaining gas doesn't match: is " ++ show (vmGasRemaining newVMState) ++ ", should be " ++ show (remainingGas test) ++ ", diff=" ++ show (vmGasRemaining newVMState - fromJust (remainingGas test))
    (_, _, _, False, _) -> do
      liftIO $ putStrLn "llllllllllllllllllllll"
      liftIO $ putStrLn $ show $ logs newVMState
      liftIO $ putStrLn "llllllllllllllllllllll"
      liftIO $ putStrLn $ show $ logs' test
      liftIO $ putStrLn "llllllllllllllllllllll"
      return $ Left "logs don't match"
    (_, _, _, _, False) -> do
      liftIO $ do
        putStrLn $ "callcreates test = " ++ show (callcreates test)
        putStrLn $ "returnedCallCreates = " ++ show (debugCallCreates newVMState)
      
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
    runStateT (runStateT (runAllTests maybeFileName maybeTestName) (Context [] 0 [] (length args == 2))) cxt

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
