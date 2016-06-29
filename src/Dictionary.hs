module Dictionary where

import Data.Word
import qualified Data.ByteString as B
import qualified Data.Binary.Strict.Get as BS
import BinUtils
import Header

--   n     list of keyboard input codes   entry-length  number-of-entries
-- byte  ------n bytes-----------------      byte         2-byte word
data DictHeader = DictHeader {
                    count :: Word8
                  , inputCodes :: [Word8]
                  , entryLength :: Word8
                  , entryCount :: Word16
}

readDictHeader :: Header -> BS.Get DictHeader
readDictHeader h = do
  count <- BS.getWord8  @@@ dictionaryLoc h
  BS.skip (fromIntegral count :: Int)
  entryLength <- BS.getWord8
  entryCount <- BS.getWord16le
  -- B.
  return $ DictHeader count [] entryLength entryCount
readDictionary = 1
