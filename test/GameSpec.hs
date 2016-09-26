module GameSpec (main, spec) where

import Graphics.UI.GLFW (Key(..))
import Test.Hspec

import Game

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Game" $ do
  describe "movement" $ do
    it "moves the character left" $
      (initialState |> placeCharacter (mkPosition 10 11) |> updateState Key'Left |> character |> position) `shouldBe` mkPosition 9 11
    it "moves the character right" $
      (initialState |> placeCharacter (mkPosition 10 11) |> updateState Key'Right |> character |> position) `shouldBe` mkPosition 11 11
    it "moves the character up" $
      (initialState |> placeCharacter (mkPosition 10 11) |> updateState Key'Up |> character |> position) `shouldBe` mkPosition 10 12
    it "moves the character down" $
      (initialState |> placeCharacter (mkPosition 10 11) |> updateState Key'Down |> character |> position) `shouldBe` mkPosition 10 10

  describe "walls" $ do
    it "lists added walls" $
      (initialState |> addWall (mkPosition 10 11) |> getWalls |> map position) `shouldBe` [mkPosition 10 11]
    it "blocks movement into walls" $
      (
        initialState
          |> placeCharacter (mkPosition 10 10)
          |> addWall (mkPosition 10 11)
          |> updateState Key'Up
          |> character
          |> position
      ) `shouldBe` mkPosition 10 10
