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
  describe "movement" $ do
    it "moves the character left" $
      (initialState |> placeCharacter (mkPosition 10 11) |> updateState Key'Left |> getCharacter |> position) `shouldBe` mkPosition 9 11
    it "moves the character right" $
      (initialState |> placeCharacter (mkPosition 10 11) |> updateState Key'Right |> getCharacter |> position) `shouldBe` mkPosition 11 11
    it "moves the character up" $
      (initialState |> placeCharacter (mkPosition 10 11) |> updateState Key'Up |> getCharacter |> position) `shouldBe` mkPosition 10 12
    it "moves the character down" $
      (initialState |> placeCharacter (mkPosition 10 11) |> updateState Key'Down |> getCharacter |> position) `shouldBe` mkPosition 10 10
