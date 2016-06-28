import Test.Hspec
import Data.Either
import qualified Data.ByteString as B
import qualified Data.Binary.Strict.Get as S

import Header

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $
    it "returns the first element of a list" $
      head [23 ..] `shouldBe` (23 :: Int)
  describe "Read Story Header" $
    it "should load file" $ do
      fi <- B.readFile "stories/minizork.z3"
      putStrLn "read"
      case fst $ S.runGet (readStory fi) fi of
        Left a -> print a
        Right a -> print a

        -- 1 `shouldBe` 1
