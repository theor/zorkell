module StorySpec where

import Debug.Trace

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import qualified Data.ByteString as B

import Utils
import Test.Hspec

import Types
import Header

import qualified BinReader as BR

type TestRes = ExceptT String IO


expHeader x = do print x 
                 version x `shouldBe` Version 3
                 flags1 x `shouldBe` 0
                 baseAddr x `shouldBe` 0x3709
                 initPc x `shouldBe` 0x37D9
                 dictionaryLoc x `shouldBe` 0x285A
                 objectTableLoc x `shouldBe` 0x03C6
                 globalVarLoc x `shouldBe` 0x02B4
                 baseStaticAddr x `shouldBe` 0x2187
                 flags2 x `shouldBe` 0
                 abbrevLoc x `shouldBe` 0x01F4

spec = describe "Read Story Header" $ do

  -- it "should load file" $ do
  --   fi <- minizork ()
  --   return ()

  it "should monad" $ do
    s <- readStory <$> minizork ()
    traceShowM s -- $ (Header.header .  $ s)
    let m = do -- traceShowM "START"
               setAt 0
              --  traceShowM "SET AT"
               getHeader
              --  traceShowM "readStory"
              --  traceShowM s
              --  exec readHeader
    either expectationFailure expHeader $ run s m

  it "should read header using BinReader" $ do
    s <- minizork ()
    -- traceShowM s -- $ (Header.header .  $ s)
    let m = do -- traceShowM "START"
               BR.setAt 0
              --  traceShowM "SET AT"
               readHeaderB
              --  traceShowM "readStory"
              --  traceShowM s
              --  exec readHeader
                  --  length x `shouldBe` 26108
                  --  checksum x `shouldBe` 55408
                  --  dynMem x `shouldBe` 8583
                  --  staticMem x `shouldBe` 43633
                    
    either expectationFailure expHeader $ BR.run s m
    -- print x
    -- runStateT m (s,0)
