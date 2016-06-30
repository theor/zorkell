module Types where

import Text.Printf
import Data.Word

newtype ByteAddr = ByteAddr Word16 deriving (Eq)
instance Show ByteAddr where
  show (ByteAddr b) = printf "0x%04X" b

instance Num ByteAddr where
  ByteAddr a + ByteAddr b = ByteAddr (a+b)
  ByteAddr a * ByteAddr b = ByteAddr (a*b)
  ByteAddr a - ByteAddr b = ByteAddr (a-b)
  abs = id
  signum = const 1
  fromInteger x =  ByteAddr (fromIntegral x :: Word16)


newtype WordAddr = WordAddr Word16 deriving (Show)
