module LambdasSpec (main, spec) where

import Test.Hspec
import Lambdas
import qualified NamedLambdas as NL
import NamedLambdas (nl)
import qualified NamelessLambdas as UL
import NamelessLambdas (ul)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "name and unname" $ do
    it "can work on a variable" $ do
      unname ["x", "y", "z"] (nl "y") `shouldBe` (Just $ ul "1")
      name   ["x", "y", "z"] (ul "1") `shouldBe` (Just $ nl "y")
    it "can work on applied variables" $ do
      unname ["x", "y", "z"] (nl "y x") `shouldBe` (Just $ ul "1 0")
      name   ["x", "y", "z"] (ul "1 0") `shouldBe` (Just $ nl "y x")
    it "can work on a variable applied to itself" $ do
      unname ["x", "y", "z"] (nl "y y") `shouldBe` (Just $ ul "1 1")
      name   ["x", "y", "z"] (ul "1 1") `shouldBe` (Just $ nl "y y")
    it "can work on variable in lambda" $ do
      unname [] (nl "lambda y y") `shouldBe` (Just $ ul "lambda 0")
      name   [] (ul "lambda 0")   `shouldBe` (Just $ nl "lambda x x")
    it "can work on complex lambda expression" $ do

      (unname [] (nl "lambda x (x (lambda z (z (lambda y (xyz)) x)))")
        `shouldBe`
        (Just $ ul "lambda 0 (lambda 0 (lambda 2 0 1) 1)"))

      (name   [] (ul "lambda 0 (lambda 0 (lambda 2 0 1) 1)")
        `shouldBe`
        (Just $ nl "lambda x (x (lambda y (y (lambda z (xzy)) x)))"))

  describe "substitute" $ do
    it "can substitute in the example from the PDF" $ do
      substitute (ul "0 (lambda 0 1 3) 2") 0 (ul "2 0") `shouldBe` (ul "2 0 (lambda 0 (3 1) 3) 2")