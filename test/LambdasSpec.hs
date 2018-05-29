module LambdasSpec (main, spec) where

import Test.Hspec
import Lambdas
import qualified NamedLambdas as NL
import qualified NamelessLambdas as UL

import Parser (ps)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "name and unname" $ do
    it "can work on a variable" $ do
      unname ["x", "y", "z"] (ps "y") `shouldBe` (Just $ ps "1")
      name   ["x", "y", "z"] (ps "1") `shouldBe` (Just $ ps "y")
    it "can work on applied variables" $ do
      unname ["x", "y", "z"] (ps "y x") `shouldBe` (Just $ ps "1 0")
      name   ["x", "y", "z"] (ps "1 0") `shouldBe` (Just $ ps "y x")
    it "can work on a variable applied to itself" $ do
      unname ["x", "y", "z"] (ps "y y") `shouldBe` (Just $ ps "1 1")
      name   ["x", "y", "z"] (ps "1 1") `shouldBe` (Just $ ps "y y")
    it "can work on variable in lambda" $ do
      unname [] (ps "lambda y y") `shouldBe` (Just $ ps "lambda 0")
      name   [] (ps "lambda 0")   `shouldBe` (Just $ ps "lambda x x")
    it "can work on complex lambda expression" $ do

      (unname [] (ps "lambda x (x (lambda z (z (lambda y (xyz)) x)))")
        `shouldBe`
        (Just $ ps "lambda 0 (lambda 0 (lambda 2 0 1) 1)"))

      (name   [] (ps "lambda 0 (lambda 0 (lambda 2 0 1) 1)")
        `shouldBe`
        (Just $ ps "lambda x (x (lambda y (y (lambda z (xzy)) x)))"))

  describe "substitute" $ do
    it "can substitute in the example from the PDF" $ do
      substitute (ps "0 (lambda 0 1 3) 2") 0 (ps "2 0") `shouldBe` (ps "2 0 (lambda 0 (3 1) 3) 2")

  describe "reduce" $ do
    it "acts like identity when the given term contains no redexes" $ do
      reduce (ps "1 lambda 0") `shouldBe` (ps "1 lambda 0")
    it "reduces a redex" $ do
      reduce (ps "(lambda 0 1) 1") `shouldBe` (ps "1 0")
    it "reduces a redex deep into the term" $ do
      -- too lazy to think about unnamed terms
      (onNamed reduce (ps "x (lambda x ((lambda x (y x)) z))"))
      `shouldBe`
      (ps "x (lambda t (y z))")