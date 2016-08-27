module Game where
import Graphics.UI.GLUT

data State = State Character Mobs

data Character = Character Int Int

data Mobs = Mobs [Mob]

data Mob = Mob Int Int Status

data Status = Alive | Dead

initialState = State (Character 0 0) (Mobs [Mob 10 10 Alive])

updateState key state = nextState key state state

class UpdateOnInput a where
  nextState :: Key -> State -> a -> a

instance UpdateOnInput State where
  nextState key state (State character mobs) =
    State (nextState key state character) (nextState key state mobs)

instance UpdateOnInput Mobs where
  nextState k s (Mobs mobs) = Mobs $ map (nextState k s) mobs

instance UpdateOnInput Mob where
  nextState _ (State (Character x1 y1) _) (Mob x2 y2 Alive) | x1 == y2 && x2 == y2 = Mob x2 y2 Dead
  nextState _ _ mob = mob

instance UpdateOnInput Character where
  nextState (SpecialKey KeyUp) _ (Character x y) = Character x (y + 1)
  nextState (SpecialKey KeyDown) _ (Character x y) = Character x (y - 1)
  nextState (SpecialKey KeyRight) _ (Character x y) = Character (x + 1) y
  nextState (SpecialKey KeyLeft) _ (Character x y) = Character (x - 1) y
  nextState _ _ x = x
