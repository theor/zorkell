module Dictionary where

import Control.Monad
import Data.Word
import qualified Data.ByteString as B
import qualified Data.Binary.Strict.Get as BS

import Types
import BinUtils
import Header
import qualified ZString

--   n     list of keyboard input codes   entry-length  number-of-entries
-- byte  ------n bytes-----------------      byte         2-byte word
data DictHeader = DictHeader {
                    count :: Word8
                  , inputCodes :: [Word8]
                  , entryLength :: Int
                  , entryCount :: Int

                  , firstEntryAddr :: ByteAddr
} deriving (Show)

--encoded text of word        bytes of data
------- 4 bytes ------   (entry length-4) bytes
type DictEntry = (String, [Word8])

data Dictionary = Dictionary { header :: DictHeader, entries :: [DictEntry] }
  deriving (Show)

dictStart :: ByteAddr -> Word8 -> ByteAddr
dictStart dictLoc count = dictLoc + 4 + (fromIntegral count :: ByteAddr)


wToInt x = fromIntegral x :: Int

readDictHeader :: ByteAddr -> BS.Get DictHeader
readDictHeader dicloc = do
  BS.skip (toInt dicloc)
  count <- BS.getWord8  @@@ dicloc
  rinputCodes <-  Control.Monad.replicateM (fromIntegral count :: Int) BS.getWord8
  -- BS.getWord8
  -- BS.skip (fromIntegral count :: Int)
  entryLength <- BS.getWord8
  entryCount <- getWord16
  return $ DictHeader count rinputCodes (wToInt entryLength) (wToInt entryCount) (dictStart dicloc count)

readEntry :: Int -> BS.Get DictEntry
readEntry entryLength = do
  etext <- BS.getByteString 4
  edata <- replicateM (entryLength - 4) BS.getWord8 -- ByteString
  return (ZString.decodeString etext,edata)


readDictionary :: DictHeader -> BS.Get Dictionary
readDictionary header = do
  let start = firstEntryAddr header
  BS.skip (toInt start)
  let entries = []
  -- entries <- replicateM 1 (readEntry . entryLength $ header)
  entries <- replicateM (entryCount header) (readEntry . entryLength $ header)
  return $ Dictionary header entries
