module ObjectTableSpec where

import qualified Data.Map as Map
import qualified Data.ByteString as B
import qualified Data.Binary.Strict.Get as S

import Utils
import Test.Hspec
import Text.Printf

import Types
import Header
import ZString
import qualified ObjectTable as OT

spec = describe "Object Table" $ do
  it "should read Object #6b" $ do
    s <- readStory <$> minizork ()
    ev (run s $ OT.readObject 0x6b)
      (\obj -> do
        print obj
        let x = OT.header obj
        OT.parent x `shouldBe` OT.ObjectNumber 0x1b
        OT.sibling x `shouldBe` OT.ObjectNumber 0x4e
        OT.child x `shouldBe` OT.ObjectNumber 0x78
        OT.properties x `shouldBe` ByteAddr 0x135B
        OT.shortName obj `shouldBe` "Egyptian Room")
  it "should read Object #2" $ do
    s <- readStory <$> minizork ()
    ev (run s $ OT.readObject 2)
      (\obj -> do
        print obj
        let x = OT.header obj
        OT.parent x `shouldBe` OT.ObjectNumber 0x1b
        OT.sibling x `shouldBe` OT.ObjectNumber 0x77
        OT.child x `shouldBe` OT.ObjectNumber 0x5f
        OT.properties x `shouldBe` ByteAddr 0x0A5D
        OT.shortName obj `shouldBe` "Up a Tree")

  it "should read object addr 1" $ do
    fi <- minizork ()
    let r = OT.objectAddr (ByteAddr 0x03C6) 1
    r `shouldBe` 0x404

  it "should read object count" $ do
    s <- readStory <$> minizork ()
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

  -- it "should fetch tree" $ do
  --   let findRoots = Map.filter (OT.invalidObject . OT.parent . OT.header)
  --
  --   s <- readStory <$> minizork ()
  --   ev (run s $ do objs <- OT.readAllObjects
  --                  return $ Map.fromAscList . map (\x -> (fromIntegral . OT.oid . OT.header $ x,x)) $ objs)
  --     (\x -> do
  --       let roots = Map.elems $ findRoots x
  --       mapM_ (putStr . draw x 0) roots)

draw :: Map.Map OT.ObjectNumber OT.Object -> Int -> OT.Object -> String
draw objs lvl x =
  concat (replicate lvl " . ")
  ++ "[" ++ printf "%3i" on ++ "] \""
  ++ OT.shortName x ++ "\"\n"
  ++ rec OT.child (+1)
  ++ rec OT.sibling id
  where
    OT.ObjectNumber on = OT.oid . OT.header $ x
    rec f lvlF = case Map.lookup (f . OT.header $ x) objs of
          Nothing -> []
          Just c -> draw objs (lvlF lvl) c
