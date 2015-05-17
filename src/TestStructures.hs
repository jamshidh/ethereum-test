{-# LANGUAGE OverloadedStrings #-}

module TestStructures where

-- External data modules
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

-- External function modules
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

-- Blockchain modules
import Blockchain.Context (putStorageKeyVal')
import Blockchain.Data.AddressStateDB (getAddressState, AddressState (..))
import Blockchain.Data.BlockDB (Block(..), BlockData(..))
import Blockchain.Data.Code (codeBytes, Code(..))
import Blockchain.Data.RLP
import Blockchain.Data.Transaction (createContractCreationTX, createMessageTX)
import Blockchain.Database.MerklePatricia.Internal (unsafeGetKeyVals, MPDB(..))
import Blockchain.DB.CodeDB (addCode, getCode)
import Blockchain.DBM (DBs(..))
import Blockchain.Format (format)
import Blockchain.Util (byteString2Integer)
import Blockchain.SHA (hash)
import Blockchain.VM.Code (getValidJUMPDESTs)
import Blockchain.VM.Environment (Environment(..))

-- internal modules
import TestDescriptions
import Conversions

populateAndConvertAddressState owner addressState' = do
  lift . addCode . codeBytes . contractCode' $ addressState'
  forM_ (Map.toList $ storage' addressState') $ \(key, val) ->
    putStorageKeyVal' owner (fromIntegral key) (fromIntegral val)
  addressState <- lift $ getAddressState owner
  return $
    AddressState
    (nonce' addressState')
    (balance' addressState')
    (addressStateContractRoot addressState)
    (hash $ codeBytes $ contractCode' addressState')

getDataAndRevertAddressState addressState = do
  theCode <- lift $ flip fmap (getCode (addressStateCodeHash addressState)) $
             fromMaybe (error $ "Missing code in getDataAndRevertAddressState: "
                        ++ format addressState)
  -- Copied wholesale from Context.hs:getAllStorageKeyVals'
  -- since that function requires an unhashed owner.
  -- This piece of code really should be in the lib somewhere
  storage <- do
    dbs <- lift get
    let mpdb = (stateDB dbs){stateRoot=addressStateContractRoot addressState}
    kvs <- lift $ lift $ unsafeGetKeyVals mpdb ""
    return $ map (fmap $ fromInteger . rlpDecode . rlpDeserialize . rlpDecode) kvs
  return $
    AddressState'
    (addressStateNonce addressState)
    (addressStateBalance addressState)
    (Map.mapKeys (byteString2Integer . nibbleString2ByteString)
     . Map.map (fromIntegral) $ Map.fromList storage)
    (Code theCode)

testExecEnv test@Test{theInput = IExec exec} =
  Environment{
    envGasPrice=getNumber $ gasPrice' exec,
    envBlock=(testBlock test),
    envOwner = address' exec,
    envOrigin = origin exec,
    envInputData = theData $ data' exec,
    envSender = caller exec,
    envValue = getNumber $ value' exec,
    envCode = code exec,
    envJumpDests = getValidJUMPDESTs $ code exec
    }
testExecEnv _ = undefined

testTransaction Test{theInput = ITransaction transaction@Transaction'{tTo' = Nothing}} =
  createContractCreationTX
  (getNumber $ tNonce' transaction)
  (getNumber $ tGasPrice' transaction)
  (getNumber $ tGasLimit' transaction)
  (getNumber $ tValue' transaction)
  (Code $ theData $ tData' transaction)
  (tSecretKey' transaction)
                      
testTransaction Test{theInput = ITransaction transaction@Transaction'{tTo' = Just a}} =
  createMessageTX
  (getNumber $ tNonce' transaction)
  (getNumber $ tGasPrice' transaction)
  (getNumber $ tGasLimit' transaction)
  a
  (getNumber $ tValue' transaction)
  (theData $ tData' transaction)
  (tSecretKey' transaction)

testTransaction _ = undefined

testBlock test=
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

