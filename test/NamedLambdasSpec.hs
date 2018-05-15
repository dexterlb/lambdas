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