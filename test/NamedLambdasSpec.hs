module NamedLambdasSpec (main, spec) where

import Test.Hspec
import NamedLambdas
import qualified Data.Set as Set

import Parser (parse)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parser" $ do
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

    it "parses application left-associatively twice" $ do
      parse "x y z t" `shouldBe` (Just
        (Application
          (Application
            (Application
              (Variable "x") (Variable "y"))
              (Variable "z"))
              (Variable "t"))
        )
  describe "fv" $ do
    it "yields correct free variables for an example term" $ do
      fv (nl "(lambda z ((lambda x y) z)) z") `shouldBe` Set.fromList ["y", "z"]

  describe "bv" $ do
    it "yields correct bound variables for an example term" $ do
      bv (nl "(lambda z ((lambda x y) z)) z") `shouldBe` Set.fromList ["x", "z"]
