-- The functions in this module don't really stand on their own.
-- They're only here for neatness.
module Debug (debugAddressStates, debugSuicideList, withDebug) where

-- External function modules
import Control.Monad.IfElse (whenM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer (tell)
import qualified Data.ByteString as ByteS (null)
import Data.Functor ((<$>))
import Data.List (intercalate)
import qualified Data.Map as Map (null, toList, map, mapKeys)
import Data.Monoid (Sum(..))
import Numeric (showHex)
import Text.PrettyPrint.ANSI.Leijen (pretty)

-- Blockchain modules
import qualified Blockchain.Colors as Color
import Blockchain.Context (isDebugEnabled)
import Blockchain.Data.Code (Code(..))
import Blockchain.Format (format)
import Blockchain.VM.VMState (VMState(..))

-- Internal modules
import TestDescriptions

showHexInt x
  = let xHex = showHex x ""
    in (if odd $ length xHex
        then "0x0"
        else "0x")
       ++ xHex

showInfo (key,val@AddressState'{nonce'=n, balance'=b, storage'=s,
                                contractCode'=Code c}) =
  show (pretty key) ++ "[#ed]" ++ "(" ++ show n ++ "): " ++ show b ++ 
  (if Map.null s
   then ""
   else ", " ++ (show $ Map.toList $
                 Map.map showHexInt $ Map.mapKeys ((++ "[#ed]") . showHexInt) s)
  ) ++ 
  (if ByteS.null c then "" else ", CODE:[" ++ Color.blue (format c) ++ "]")

debugAddressStates before after expected = 
  whenM isDebugEnabled $ do
    liftIO $ putStrLn "Before-------------"
    liftIO $ putStrLn $ unlines $ showInfo <$> before
    liftIO $ putStrLn "After-------------"
    liftIO $ putStrLn $ unlines $ showInfo <$> after
    liftIO $ putStrLn "Expected-------------"
    liftIO $ putStrLn $ unlines $ showInfo <$> expected
    liftIO $ putStrLn "-------------"

debugSuicideList vmState =
  whenM (lift $ lift isDebugEnabled) $ do
    liftIO $ putStrLn $ "Removing accounts in suicideList: " ++
      intercalate ", " (show . pretty <$> suicideList vmState)

withDebug counts color (messS, debugS) =
  (lift $ do
    liftIO $ putStrLn (color messS)
    debugEnabled <- isDebugEnabled
    if (debugEnabled && not(null debugS))
      then liftIO $ putStrLn debugS
      else return ())
  >> tell counts
  
