module Dictionary where

import Debug.Trace

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

readDictHeader :: StoryReader DictHeader
readDictHeader = do
  h <- getHeader
  let dictAddr = dictionaryLoc h
  setAtAddr dictAddr
  vIsAt <- ByteAddr . fromIntegral <$> isAt
  traceShowM ("set at", vIsAt)
  exec $ do
    bread <- BS.bytesRead
    count <- BS.getWord8
    traceShowM ("sep count", count, bread)
    let firstEntryAddr = dictStart dictAddr count
    rinputCodes <-  Control.Monad.replicateM (fromIntegral count) BS.getWord8
  -- BS.getWord8
  -- BS.skip (fromIntegral count :: Int)
    entryLength <- fromIntegral <$> BS.getWord8
    entryCount <- fromIntegral <$> getWord16
    return $ DictHeader count rinputCodes entryLength entryCount firstEntryAddr

readEntry :: Int -> StoryReader DictEntry
readEntry entryLength = do
  pos <- isAt
  -- traceShowM ("read entry, at", pos)
  etext <- ZString.decodeString
  setAt (pos + 4)
  edata <- exec $ replicateM (entryLength - 4) BS.getWord8

  return (etext,edata)


readDictionary :: DictHeader -> StoryReader Dictionary
readDictionary header = do
  let start = firstEntryAddr header
  -- traceShowM ("Dict first entry", start)
  setAtAddr start
  -- entries <- replicateM 1 (readEntry . entryLength $ header)
  entries <- replicateM (entryCount header) (readEntry . entryLength $ header)
  -- entries <- replicateM 3 (readEntry . entryLength $ header)
  return $ Dictionary header entries
