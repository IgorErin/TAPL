module Main(main) where

import Test.Tasty

import qualified Tests.Arith as Arith (tests)
import qualified Tests.Lambda.All as Lambda (tests)

tests :: IO TestTree
tests = testGroup "Main" <$> sequence [
    return Arith.tests,
    Lambda.tests ]

main :: IO ()
main = defaultMain =<< tests
