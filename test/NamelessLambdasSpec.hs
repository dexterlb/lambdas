module NamelessLambdasSpec (main, spec) where

import Test.Hspec
import NamelessLambdas

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parse" $ do
    it "can parse a variable" $ do
      parse "42" `shouldBe` (Just (Variable 42))
    it "can parse a simple application" $ do
      parse "1 2" `shouldBe` (Just (Application (Variable 1) (Variable 2)))
    it "can parse multi-digit application" $ do
      parse "12 25" `shouldBe` (Just (Application (Variable 12) (Variable 25)))
    it "can parse a simple lambda" $ do
      parse "lambda 5" `shouldBe` (Just (Lambda (Variable 5)))
    it "can parse a greek lambda" $ do
      parse "λ 42" `shouldBe` (Just (Lambda (Variable 42)))
    it "can parse a haskell lambda" $ do
      parse "\\ 42" `shouldBe` (Just (Lambda (Variable 42)))
    it "can parse a complex expression" $ do
      parse "lambda (lambda (lambda ((2 0) (1 0))))"
        `shouldBe` (Just
          (Lambda (Lambda (Lambda
            (Application
              (Application (Variable 2) (Variable 0))
              (Application (Variable 1) (Variable 0))
            )
          )))
        )
    it "parses application left-associatively" $ do
      parse "1 2 3" `shouldBe` (Just
        (Application
          (Application (Variable 1) (Variable 2))
          (Variable 3)
        ))
    it "parses application left-associatively twice" $ do
      parse "1 2 3 4" `shouldBe` (Just
        (Application
          (Application
            (Application
              (Variable 1) (Variable 2))
              (Variable 3))
              (Variable 4))
        )