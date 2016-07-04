{-# LANGUAGE BinaryLiterals #-}

module ZStringSpec where

import Test.Hspec
import Data.Either
import Data.Word
import qualified Data.ByteString as B
import qualified Data.Binary.Strict.Get as S
import qualified Data.Binary.Strict.BitGet as BG
import Control.Monad
import Control.Monad.Except
import Text.Printf

import Types
import Header
import ZString

spec = describe "Decode zstring" $ do

  it "should unpack zstring el" $
    case BG.runBitGet (B.pack [0x65, 0xaa]) ZString.decode of
      Left a -> expectationFailure a
      Right a -> a `shouldBe` (False, [0x19 :: Word8, 0x0d :: Word8, 0x0a :: Word8])

  it "should unpack zstring" $
    case BG.runBitGet (B.pack [0b11010011,0b00100001]) ZString.decode of
      Left a -> expectationFailure a
      Right a -> a `shouldBe` (True, [0b10100 :: Word8, 0b11001 :: Word8, 0b1:: Word8])

  it "should decode zstring 'the'" $
    ZString.decodeString (B.pack [0x65, 0xaa]) `shouldBe` "the"

  it "should decode zstring 'the '" $
    ZString.decodeString (B.pack [0x65, 0xaa, 0x80, 0xa5]) `shouldBe` "the "
