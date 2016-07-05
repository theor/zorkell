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

type TestRes = ExceptT String IO

spec = describe "Read Story Header" $ do

  it "should load file" $ do
    fi <- minizork ()
    return ()

  it "should monad" $ do
    s <- readStory <$> minizork ()
    traceShowM s -- $ (Header.header .  $ s)
    let m = do -- traceShowM "START"
               setAt 0
              --  traceShowM "SET AT"
              --  readStory fi
              --  traceShowM "readStory"
              --  traceShowM s
              --  exec readHeader
    either expectationFailure print $ run s m
    -- print x
    -- runStateT m (s,0)
