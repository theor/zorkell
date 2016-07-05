{-# LANGUAGE BinaryLiterals #-}

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
import OpCodes
import qualified ObjectTable as OT
import qualified Dictionary

import qualified ZStringSpec
import qualified StorySpec
import qualified ObjectTableSpec
import qualified DictionarySpec

minizork () = B.readFile "stories/minizork.z3"

ev = flip $ either expectationFailure
runGet fi = fst . flip S.runGet fi


main :: IO ()
main = hspec $ do

  StorySpec.spec
  ZStringSpec.spec
  ObjectTableSpec.spec
  DictionarySpec.spec
  --
  -- describe "Opcodes" $
  --   it "should decode short form" $
  --     BG.runBitGet (B.pack [0b10000000]) OpCodes.readForm `shouldBe` Right Short

-- stateT BS.get
-- state addr*ByteString
-- put @addr
-- x <- lift BS.getWord8
