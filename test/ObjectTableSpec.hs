module ObjectTableSpec where

import qualified Data.ByteString as B
import qualified Data.Binary.Strict.Get as S

import Utils
import Test.Hspec

import Types
import Header
import ZString
import qualified ObjectTable as OT


spec = describe "Object Table" $ do
  it "monad should read Object #2" $ do
    s <- readStory <$> minizork ()
    let x = run s (return ())
    either expectationFailure print x


  it "should read Object #2" $ do
    s <- readStory <$> minizork ()
    ev (run s $ OT.readObjectHeader 2)
    -- let r = runGet fi (OT.readObjectHeader (ByteAddr 0x03C6) 2)

      (\x -> do
        print x
        OT.parent x `shouldBe` OT.ObjectNumber 0x1b
        OT.sibling x `shouldBe` OT.ObjectNumber 0x77
        OT.child x `shouldBe` OT.ObjectNumber 0x5f
        OT.properties x `shouldBe` ByteAddr 0x0A5D)

  it "should read object addr 1" $ do
    fi <- minizork ()
    let r = OT.objectAddr (ByteAddr 0x03C6) 1
    r `shouldBe` 0x404

  it "should read object count" $ do
    s <- readStory <$> minizork ()
    let x = run s (return ())
    ev
      (run s OT.objectCount)
      (`shouldBe` 179)

  it "should read object table" $ do
    s <- readStory <$> minizork ()
    ev (run s OT.readAllObjects)
      (\x -> do
        Prelude.length x `shouldBe` 179
        OT.shortName (head x) `shouldBe` "forest"
        OT.shortName (x !! 1) `shouldBe` "Up a Tree"
        OT.shortName (x !! 178) `shouldBe` "pseudo")

  it "should read object properties header" $ do
    s <- readStory <$> minizork ()
    ev
      (run s $ do { setAt 0x0A5D; OT.readPropHeader })
      (\x -> do
        x `shouldBe` (8,"Up a Tree")
        print x)
