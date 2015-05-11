-- This module contains miscellaneous "convertXToY" type functions
-- It makes no sense and should be replaced with stronger type classes
module Conversions (makeHashed, getNumber, nibbleString2ByteString) where

-- External function modules
import qualified Crypto.Hash.SHA3 as SHA3
import qualified Data.Binary as Bin
import qualified Data.ByteString as ByteS
import qualified Data.ByteString.Lazy as ByteSL
import qualified Data.Map as Map
import qualified Data.NibbleString as NibbS

-- Blockchain modules
import Blockchain.Data.Address (Address(..))
import Blockchain.Util (byteString2Integer)
import Blockchain.ExtWord (word256ToBytes)

-- Internal modules
import TestDescriptions

-- This probably should be replaced by a better key-storage system
-- where we remember the hashes of the actual keys
makeHashed::(Map.Map Address AddressState') -> [(Address, AddressState')]
makeHashed m =
  Map.toList $ Map.mapKeys hashAddress $
  flip Map.map m $ \s' ->
  s'{storage' = Map.mapKeys hashInteger (storage' s')}

hashInteger
  = byteString2Integer . nibbleString2ByteString . NibbS.EvenNibbleString .
    (SHA3.hash 256) . nibbleString2ByteString . NibbS.pack .
    (NibbS.byte2Nibbles =<<) . word256ToBytes . fromIntegral

hashAddress (Address s)
  = Address $ fromIntegral $ byteString2Integer $
    nibbleString2ByteString $ NibbS.EvenNibbleString $ (SHA3.hash 256) $
    ByteSL.toStrict $ Bin.encode s

nibbleString2ByteString::NibbS.NibbleString -> ByteS.ByteString
nibbleString2ByteString (NibbS.EvenNibbleString str) = str
nibbleString2ByteString (NibbS.OddNibbleString c str) = c `ByteS.cons` str

getNumber "" = 0
getNumber x = read x
