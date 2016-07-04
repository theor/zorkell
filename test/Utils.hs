module Utils where

import Data.Either
import qualified Data.ByteString as B
import qualified Data.Binary.Strict.Get as S

import Test.Hspec

minizork () = B.readFile "stories/minizork.z3"
ev = flip $ either expectationFailure
runGet fi = fst . flip S.runGet fi
