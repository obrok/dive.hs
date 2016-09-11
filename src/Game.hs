module Game where

import Graphics.UI.GLFW

data Character = Character Int Int
  deriving (Show)

data Mobs = Mobs [Mob]
  deriving (Show)

data Mob = Mob Int Int Status
  deriving (Show)

data Status = Alive | Dead
  deriving (Show)

data State = State { character :: Character, mobs :: Mobs }
  deriving (Show)

initialState :: State
initialState = State (Character 0 0) (Mobs [Mob 10 10 Alive])

updateState :: Key -> State -> State
updateState key state = nextState key state state

getCharacter :: State -> Character
getCharacter State { character } = character

getMobs :: State -> [Mob]
getMobs State { mobs = Mobs ms } = ms

getWalls :: State -> [(Int, Int)]
getWalls _ = [(20, 20), (20, 21), (21, 20)]

class UpdateOnInput a where
  nextState :: Key -> State -> a -> a

instance UpdateOnInput State where
  nextState key state (State character mobs) =
    State (nextState key state character) (nextState key state mobs)

instance UpdateOnInput Mobs where
  nextState k s (Mobs mobs) = Mobs $ map (nextState k s) mobs

instance UpdateOnInput Mob where
  nextState _ (State (Character x1 y1) _) (Mob x2 y2 Alive) | x1 == x2 && y1 == y2 = Mob x2 y2 Dead
  nextState _ _ mob = mob

instance UpdateOnInput Character where
  nextState Key'Up _ (Character x y) = Character x (y + 1)
  nextState Key'Down _ (Character x y) = Character x (y - 1)
  nextState Key'Right _ (Character x y) = Character (x + 1) y
  nextState Key'Left _ (Character x y) = Character (x - 1) y
  nextState _ _ x = x
