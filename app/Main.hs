module Main where

import Lib
import qualified Data.ByteString as B

main :: IO ()
main = do
  input <- B.getContents
  -- readStory input
  putStrLn "hello world"
