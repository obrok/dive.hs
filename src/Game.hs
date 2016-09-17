module Game where

import Graphics.UI.GLFW (Key(..))

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

newtype GameTime = GameTime Int

initialState :: State
initialState = State (Character 0 0) (Mobs [Mob 10 10 Alive])

updateState :: Key -> State -> State
updateState key state =
  let (time, state') = applyAction key state
    in nextState time state'

getCharacter :: State -> Character
getCharacter State { character } = character

getMobs :: State -> [Mob]
getMobs State { mobs = Mobs ms } = ms

getWalls :: State -> [(Int, Int)]
getWalls _ = [(20, 20), (20, 21), (21, 20)]

nextState :: GameTime -> State -> State
nextState _time state = state

applyAction :: Key -> State -> (GameTime, State)
applyAction Key'Up state@State {character = (Character x y)} = (GameTime 0, state{character = Character x (y + 1)})
applyAction Key'Down state@State {character = (Character x y)} = (GameTime 0, state{character = Character x (y - 1)})
applyAction Key'Left state@State {character = (Character x y)} = (GameTime 0, state{character = Character (x - 1) y})
applyAction Key'Right state@State {character = (Character x y)} = (GameTime 0, state{character = Character (x + 1) y})
applyAction _ x = (GameTime 0, x)
