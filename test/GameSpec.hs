module GameSpec (main, spec) where

import Graphics.UI.GLFW (Key(..))
import Test.Hspec

import Game

main :: IO ()
main = hspec spec

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

spec :: Spec
spec = describe "Game" $
  describe "movement" $
    it "moves the character left" $
      (initialState |> placeCharacter (mkPosition 10 11) |> updateState Key'Left |> getCharacter |> position) `shouldBe` mkPosition 9 11
