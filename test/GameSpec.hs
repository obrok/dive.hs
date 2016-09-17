module GameSpec (main, spec) where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Game" $ do
  it "asserts the truth" $ do
    True `shouldBe` True
