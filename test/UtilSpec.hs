{-# LANGUAGE NoImplicitPrelude #-}
module UtilSpec (spec) where

import Import
import Util
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "chunk" $ do
    it "chunks" $ chunk 3 [1::Integer,2,3,4,5] `shouldBe` [[1,2,3], [4,5]]
