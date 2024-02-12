module Main(main) where

import Test.Tasty

import qualified Arith (tests)

main :: IO ()
main = defaultMain Arith.tests
