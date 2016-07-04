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

spec = describe "Dictionary" $ do
  it "tmp" $ print 0
  -- it "should read dict header" $ do
  --   fi <- minizork ()
  --   either expectationFailure id $ do
  --     s <- readStory fi
  --     h <- runStoryRead (Dictionary.readDictHeader (ByteAddr 0x285A)) $ s
  --     return $ print h
  -- it "should read dict entries" $ do
  --   fi <- minizork ()
  --   let r = do eh <- fst . S.runGet (Dictionary.readDictHeader (ByteAddr 0x285A)) $ fi
  --              fst . S.runGet (Dictionary.readDictionary eh) $ fi
  --
  --   ev r $ \x -> do
  --     print . Dictionary.header $ x
  --     let f = map print (take 10 . Dictionary.entries $ x)
  --     sequence_ f
