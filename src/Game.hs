{-# LANGUAGE NamedFieldPuns, GeneralizedNewtypeDeriving #-}

module Game where

import Graphics.UI.GLFW (Key(..))

newtype Character = Character Position
  deriving (Show, Positioned)

newtype Mobs = Mobs [Mob]
  deriving (Show)

newtype Walls = Walls { rawWalls :: [Wall] }
  deriving (Show)

newtype Wall = Wall Position
  deriving (Show, Positioned)

newtype Mob = Mob Position
  deriving (Show, Positioned)

data Status = Alive | Dead
  deriving (Show)

data State = State { character :: Character, mobs :: Mobs, walls :: Walls }
  deriving (Show)

newtype GameTime = GameTime Int

newtype Position = Position { rawPosition :: (Int, Int) }
  deriving (Eq, Show)

class Positioned a where
  position :: a -> Position

instance Positioned Position where
  position = id

initialState :: State
initialState = State (Character $ mkPosition 0 0) (Mobs [Mob $ mkPosition 10 10]) (Walls [])

updateState :: Key -> State -> State
updateState key state =
  let (time, state') = applyAction key state
    in if valid state' then nextState time state' else state

valid :: State -> Bool
valid state = all (\w -> position w /= position (character state)) (rawWalls $ walls state)

placeCharacter :: Position -> State -> State
placeCharacter p s = s { character = Character p }

getMobs :: State -> [Mob]
getMobs State { mobs = Mobs ms } = ms

getWalls :: State -> [Wall]
getWalls = rawWalls . walls

nextState :: GameTime -> State -> State
nextState _time state = state

applyAction :: Key -> State -> (GameTime, State)
applyAction k s = (GameTime 0, s { character = Character (move k . position . character $ s) })
  where move Key'Up (Position (x, y)) = Position (x, y + 1)
        move Key'Down (Position (x, y)) = Position (x, y - 1)
        move Key'Left (Position (x, y)) = Position (x - 1, y)
        move Key'Right (Position (x, y)) = Position (x + 1, y)
        move _ p = p

mkPosition :: Int -> Int -> Position
mkPosition x y = Position (x, y)

addWall :: Position -> State -> State
addWall pos state@State{ walls = (Walls ws) } = state{ walls = Walls (Wall pos : ws) }

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)
