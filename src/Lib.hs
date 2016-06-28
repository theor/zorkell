module Lib where

import qualified Data.ByteString as B
import qualified Data.Binary.Strict.Get as BS
f = 2

readStory x =
  BS.getWord16be
