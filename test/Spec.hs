{-# LANGUAGE BinaryLiterals #-}

import Test.Hspec
import Data.Either
import Data.Word
import qualified Data.ByteString as B
import qualified Data.Binary.Strict.Get as S
import qualified Data.Binary.Strict.BitGet as BG
import Control.Monad

import Types
import Header
import ZString
import OpCodes
import ObjectTable
import qualified Dictionary

minizork () = B.readFile "stories/minizork.z3"

ev = flip $ either expectationFailure

main :: IO ()
main = hspec $ do

  describe "Decode zstring" $ do

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

  describe "Prelude.head" $
    it "returns the first element of a list" $
      head [23 ..] `shouldBe` (23 :: Int)

  describe "Read Story Header" $

    it "should load file" $ do
      fi <- minizork ()
      case readStory fi of
        Left a -> expectationFailure a
        Right a -> print $ header a

  describe "Opcodes" $
    it "should decode short form" $
      BG.runBitGet (B.pack [0b10000000]) OpCodes.readForm `shouldBe` Right Short

  describe "Dictionary" $ do
    it "should read dict header" $ do
      fi <- minizork ()
      case fst . S.runGet (Dictionary.readDictHeader (ByteAddr 0x285A)) $ fi of
        Left a -> expectationFailure a
        Right a -> print a

    it "should read dict entries" $ do
      fi <- minizork ()
      let r = do eh <- fst . S.runGet (Dictionary.readDictHeader (ByteAddr 0x285A)) $ fi
                 fst . S.runGet (Dictionary.readDictionary eh) $ fi

      ev r $ \x -> do
        print . Dictionary.header $ x
        let f = map print (take 10 . Dictionary.entries $ x)
        sequence_ f

  describe "Object Table" $ do
    it "should read table" $ do
      fi <- minizork ()
      let r = fst . S.runGet (ObjectTable.readObject (ByteAddr 0x03C6) 2) $ fi

      ev r $ \x -> do
        print x
        parent x `shouldBe` ObjectNumber 0x1b
        sibling x `shouldBe` ObjectNumber 0x77
        child x `shouldBe` ObjectNumber 0x5f
        properties x `shouldBe` ByteAddr 0x0A5D

    it "should read table" $ do
      fi <- minizork ()
      let r = fst . S.runGet (ObjectTable.readPropHeader (ByteAddr 0x0A5D)) $ fi

      ev r $ \x -> do
        print x
        -- parent x `shouldBe` ObjectNumber 0x1b
        -- sibling x `shouldBe` ObjectNumber 0x77
        -- child x `shouldBe` ObjectNumber 0x5f
        -- properties x `shouldBe` ByteAddr 0x0A5D
