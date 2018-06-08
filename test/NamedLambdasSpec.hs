module NamedLambdasSpec (main, spec) where

import Test.Hspec
import NamedLambdas
import qualified Data.Set as Set

import Parser (ps)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parser" $ do
    it "can parse a variable" $ do
      ps "x" `shouldBe` (Variable "x")
    it "can parse a simple application" $ do
      ps "xy" `shouldBe` (Application (Variable "x") (Variable "y"))
    it "can parse a simple lambda" $ do
      ps "lambda x y" `shouldBe` (Lambda "x" (Variable "y"))
    it "can parse a greek lambda" $ do
      ps "λx y" `shouldBe` (Lambda "x" (Variable "y"))
    it "can parse a haskell lambda" $ do
      ps "\\x y" `shouldBe` (Lambda "x" (Variable "y"))
    it "can parse a complex term" $ do
      ps "lambda x (lambda y (lambda z ((xz) (yz))))"
        `shouldBe`
          (Lambda "x" (Lambda "y" (Lambda "z"
            (Application
              (Application (Variable "x") (Variable "z"))
              (Application (Variable "y") (Variable "z"))
            )
          )))

    it "parses application left-associatively" $ do
      ps "xyz" `shouldBe`
        (Application
          (Application (Variable "x") (Variable "y"))
          (Variable "z")
        )

    it "parses application left-associatively twice" $ do
      ps "x y z t" `shouldBe`
        (Application
          (Application
            (Application
              (Variable "x") (Variable "y"))
              (Variable "z"))
              (Variable "t"))
  describe "fv" $ do
    it "yields correct free variables for an example term" $ do
      fv (ps "(lambda z ((lambda x y) z)) z") `shouldBe` Set.fromList ["y", "z"]

  describe "bv" $ do
    it "yields correct bound variables for an example term" $ do
      bv (ps "(lambda z ((lambda x y) z)) z") `shouldBe` Set.fromList ["x", "z"]
