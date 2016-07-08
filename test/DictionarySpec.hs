module DictionarySpec where


import qualified Data.ByteString as B
import qualified Data.Binary.Strict.Get as S

import Test.Hspec
import Utils

import Types
import Header
import ZString
import qualified Dictionary
import qualified ObjectTable as OT

spec :: Spec
spec = describe "Dictionary" $ do
  it "tmp" $ print 0

  it "should read dict header" $ do
    s <- readStory <$> minizork ()
    ev (run s Dictionary.readDictHeader)
      (\x -> do print x
                Dictionary.count x `shouldBe` 3)

  it "should read dict entries" $ do
    s <- readStory <$> minizork ()
    ev (run s $ do eh <- Dictionary.readDictHeader
                   Dictionary.readDictionary eh) $ \x -> do
      print . Dictionary.header $ x
      let p = print
      let f = map p (take 10 . Dictionary.entries $ x)
      sequence_ f
