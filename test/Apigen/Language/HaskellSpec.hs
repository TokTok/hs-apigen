{-# LANGUAGE OverloadedStrings #-}
module Apigen.Language.HaskellSpec where

import           Test.Hspec          (Spec, it, shouldBe)


variable :: Int
variable = 123


spec :: Spec
spec = do
    it "should work" $ do
        variable `shouldBe` variable
