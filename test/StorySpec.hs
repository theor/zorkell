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
  -- let itt :: String -> IO (Either String b) -> SpecWith (Arg (b))
  let compute success = if success then Right else const $ Left "Error"
  let qq :: TestRes ()
      qq = do throwError "asdasd"
              b <- liftIO $ minizork ()
              -- let c = lift $ compute True b
              liftIO $ print 1
  let qqq = runExceptT qq
  let qqqq = do e <- qqq
                either expectationFailure return e
  let itt s x = it s $ either fail id x
      -- itt s x =  it s $ do res <- x
      --                     return $ either fail lift res
  let asd = either expectationFailure id . runExcept
  -- it "asd" $ do
  --   asd $ do
  --     return $ do
  --       fi <- minizork ()
  --       print 0
  --       print 0
  -- it "test" $ do
  --   -- fake computation
  --   let compute success = if success then Right else const $ Left "Error"
  --   f <- B.readFile "some/file"
  --   either fail id $ do a <- compute True f
  --                       b <- compute False a
  --                       return $ do a `shouldBe` a
  --                                   b `shouldBe` b

  -- let f = minizork () `catchError` print
  it "should load file" $ do
    fi <- minizork ()
    -- do { readStory fi } `catchError` expectationFailure
    -- a <- runExceptT f
    -- let x = either fail print $ readStory fi
    -- either expectationFailure id a
    return ()
    -- print 1
  -- it "should load file" $ do
  --   fi <- minizork ()
  --   case readStory fi of
  --     Left a -> expectationFailure a
  --     Right a -> print $ header a
  --
  it "should monad" $ do
    fi <- minizork ()
    let m = do traceShowM "START"
               setAt 0
               traceShowM "SET AT"
               readStory fi
               traceShowM "readStory"
              --  traceShowM s
              --  exec readHeader
    let x = runStateT m (Left "Not loaded", 0)
    either expectationFailure print x
    -- print x
    -- runStateT m (s,0)
