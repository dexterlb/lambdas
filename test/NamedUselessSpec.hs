module NamedUselessSpec (main, spec) where

import Test.Hspec
import NamedLambdas
import NamedUseless

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "bvrename" $ do
    it "can rename direct bound expression" $ do
      bvrename (nl "lambda x (xy)") "x" "z" `shouldBe` (nl "lambda z (zy)")

    it "can rename direct unbound expression" $ do
      bvrename (nl "lambda x (xy)") "y" "z" `shouldBe` (nl "lambda x (xy)")

    it "can rename variable in both bound and unbound contexts" $ do
      bvrename (nl "x (lambda x (xy))") "x" "z" `shouldBe` (nl "x (lambda z (zy))")

  describe "substitute" $ do
    it "can substitute on the example from the PDF" $ do
      substitute (nl "y (lambda x (xyz)) z") "y" (nl "zy")
      `shouldBe`
      (nl "zy (lambda x (x(zy)z)) z")
    it "can substitute when there's a clash of variables" $ do
      substitute (nl "y (lambda x (xyz)) z") "y" (nl "zx")
      `shouldBe`
      (nl "zx (lambda t (t(zx)z)) z")