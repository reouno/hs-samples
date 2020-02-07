module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger nat = natToInteger' nat 0

natToInteger' :: Nat -> Integer -> Integer
natToInteger' Zero       n = n
natToInteger' (Succ nat) n = natToInteger' nat n + 1
