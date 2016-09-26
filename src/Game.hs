{-# LANGUAGE NamedFieldPuns, GeneralizedNewtypeDeriving #-}

module Game where

import Graphics.UI.GLFW (Key(..))

data Character = Character Int Int
  deriving (Show)

newtype Mobs = Mobs [Mob]
  deriving (Show)

newtype Walls = Walls { rawWalls :: [Wall] }
  deriving (Show)

newtype Wall = Wall Position
  deriving (Show, Positioned)

data Mob = Mob Int Int Status
  deriving (Show)

data Status = Alive | Dead
  deriving (Show)

data State = State { character :: Character, mobs :: Mobs, walls :: Walls }
  deriving (Show)

newtype GameTime = GameTime Int

newtype Position = Position { rawPosition :: (Int, Int) }
  deriving (Eq, Show)

class Positioned a where
  position :: a -> Position

instance Positioned Character where
  position (Character x y) = Position (x, y)

instance Positioned Position where
  position = id

initialState :: State
initialState = State (Character 0 0) (Mobs [Mob 10 10 Alive]) (Walls [])

updateState :: Key -> State -> State
updateState key state =
  let (time, state') = applyAction key state
    in if valid state' then nextState time state' else state

valid :: State -> Bool
valid state = all (\w -> position w /= position (character state)) (rawWalls $ walls state)

getCharacter :: State -> Character
getCharacter State { character } = character

placeCharacter :: Position -> State -> State
placeCharacter (Position (x', y')) state = state { character = Character x' y' }

getMobs :: State -> [Mob]
getMobs State { mobs = Mobs ms } = ms

getWalls :: State -> [(Int, Int)]
getWalls = fmap (rawPosition . position) . rawWalls . walls

nextState :: GameTime -> State -> State
nextState _time state = state

applyAction :: Key -> State -> (GameTime, State)
applyAction Key'Up state@State{character = (Character x y)} = (GameTime 0, state{character = Character x (y + 1)})
applyAction Key'Down state@State{character = (Character x y)} = (GameTime 0, state{character = Character x (y - 1)})
applyAction Key'Left state@State{character = (Character x y)} = (GameTime 0, state{character = Character (x - 1) y})
applyAction Key'Right state@State{character = (Character x y)} = (GameTime 0, state{character = Character (x + 1) y})
applyAction _ x = (GameTime 0, x)

mkPosition :: Int -> Int -> Position
mkPosition x y = Position (x, y)

addWall :: Position -> State -> State
addWall pos state@State{ walls = (Walls ws) } = state{ walls = Walls (Wall pos : ws) }

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)
