module NamedLambdasSpec (main, spec) where

import Test.Hspec
import NamedLambdas

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parse" $ do
    it "can parse a variable" $ do
      parse "x" `shouldBe` (Just (Variable "x"))
    it "can parse a simple application" $ do
      parse "xy" `shouldBe` (Just (Application (Variable "x") (Variable "y")))
    it "can parse a simple lambda" $ do
      parse "lambda x y" `shouldBe` (Just (Lambda "x" (Variable "y")))
    it "can parse a greek lambda" $ do
      parse "λx y" `shouldBe` (Just (Lambda "x" (Variable "y")))
    it "can parse a haskell lambda" $ do
      parse "\\x y" `shouldBe` (Just (Lambda "x" (Variable "y")))
    it "can parse a complex expression" $ do
      parse "lambda x (lambda y (lambda z ((xz) (yz))))"
        `shouldBe` (Just
          (Lambda "x" (Lambda "y" (Lambda "z"
            (Application
              (Application (Variable "x") (Variable "z"))
              (Application (Variable "y") (Variable "z"))
            )
          )))
        )
    it "parses application left-associatively" $ do
      parse "xyz" `shouldBe` (Just
        (Application
          (Application (Variable "x") (Variable "y"))
          (Variable "z")
        ))