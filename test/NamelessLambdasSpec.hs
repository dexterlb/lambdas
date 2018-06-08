module NamelessLambdasSpec (main, spec) where

import Test.Hspec
import NamelessLambdas

import Parser (ps)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parser" $ do
    it "can parse a variable" $ do
      ps "42" `shouldBe` (Variable 42)
    it "can parse a simple application" $ do
      ps "1 2" `shouldBe` (Application (Variable 1) (Variable 2))
    it "can parse multi-digit application" $ do
      ps "12 25" `shouldBe` (Application (Variable 12) (Variable 25))
    it "can parse a simple lambda" $ do
      ps "lambda 5" `shouldBe` (Lambda (Variable 5))
    it "can parse a greek lambda" $ do
      ps "λ 42" `shouldBe` (Lambda (Variable 42))
    it "can parse a haskell lambda" $ do
      ps "\\ 42" `shouldBe` (Lambda (Variable 42))
    it "can parse a complex term" $ do
      ps "lambda (lambda (lambda ((2 0) (1 0))))"
        `shouldBe`
          (Lambda (Lambda (Lambda
            (Application
              (Application (Variable 2) (Variable 0))
              (Application (Variable 1) (Variable 0))
            )
          )))
    it "parses application left-associatively" $ do
      ps "1 2 3" `shouldBe`
        (Application
          (Application (Variable 1) (Variable 2))
          (Variable 3)
        )
    it "parses application left-associatively twice" $ do
      ps "1 2 3 4" `shouldBe`
        (Application
          (Application
            (Application
              (Variable 1) (Variable 2))
              (Variable 3))
              (Variable 4))