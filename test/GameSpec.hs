module GameSpec (main, spec) where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Game" $
  it "asserts the truth" $
    True `shouldBe` True
