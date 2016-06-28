{-# LANGUAGE BinaryLiterals #-}

import Test.Hspec
import Data.Either
import Data.Word
import qualified Data.ByteString as B
import qualified Data.Binary.Strict.Get as S
import qualified Data.Binary.Strict.BitGet as BG

import Header
import ZString

main :: IO ()
main = hspec $ do

  describe "Decode zstring" $ do

    it "should unpack zstring el" $
      case BG.runBitGet (B.pack [0x65, 0xaa]) ZString.decode of
        Left a -> expectationFailure a
        Right a -> a `shouldBe` (False, [0x19 :: Word8, 0x0d :: Word8, 0x0a :: Word8])

    it "should unpack zstring el" $
      ZString.decodeString (B.pack [0x65, 0xaa]) `shouldBe` "the"
    it "should unpack zstring el" $
      ZString.decodeString (B.pack [0x65, 0xaa, 0x80, 0xa5]) `shouldBe` "the "
    it "should unpack zstring" $
      case BG.runBitGet (B.pack [0b11010011,0b00100001]) ZString.decode of
        Left a -> expectationFailure a
        Right a -> a `shouldBe` (True, [0b10100 :: Word8, 0b11001 :: Word8, 0b1:: Word8])

  describe "Prelude.head" $
    it "returns the first element of a list" $
      head [23 ..] `shouldBe` (23 :: Int)

  describe "Read Story Header" $

    it "should load file" $ do
      fi <- B.readFile "stories/minizork.z3"
      putStrLn "read"
      case fst $ S.runGet (readStory fi) fi of
        Left a -> expectationFailure a
        Right a -> print a
