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
  it "should read Object #2" $ do
    fi <- minizork ()
    let r = runGet fi (OT.readObjectHeader (ByteAddr 0x03C6) 2)

    ev r $ \x -> do
      print x
      OT.parent x `shouldBe` OT.ObjectNumber 0x1b
      OT.sibling x `shouldBe` OT.ObjectNumber 0x77
      OT.child x `shouldBe` OT.ObjectNumber 0x5f
      OT.properties x `shouldBe` ByteAddr 0x0A5D

  it "should read object addr 1" $ do
    fi <- minizork ()
    let r = OT.objectAddr (ByteAddr 0x03C6) 1
    r `shouldBe` 0x404

  it "should read object count" $ do
    fi <- minizork ()
    let r = runGet fi (OT.objectCount (ByteAddr 0x03C6))
    ev r $ \x -> do print x
                    x `shouldBe` 179
    -- printf "%i" c

  it "should read object table" $ do
    fi <- minizork ()
    let c = runGet fi (OT.objectCount (ByteAddr 0x03C6))
    let cc = either (const 0) id c
    -- let cc = 10
    let i = [1,2..cc]
    let r fi i = do obj <- runGet fi . OT.readObjectHeader (ByteAddr 0x03C6) $ i
                    hea <- runGet fi $ OT.readPropHeader (OT.properties obj)
                    return (hea, obj)
    let asd = map (r fi) i
    mapM_ (`ev` print) asd
  --  $ i
  -- let r = runGet fi . ObjectTable.readObject (ByteAddr 0x03C6) $ 2
  -- print c


  it "should read object properties header" $ do
    fi <- minizork ()
    let r = runGet fi (OT.readPropHeader (ByteAddr 0x0A5D))

    ev r $ \x -> do
      x `shouldBe` (8,"Up a Tree")
      print x
