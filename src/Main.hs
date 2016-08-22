import Graphics.UI.GLUT
import Data.IORef

data State = State Integer Integer
  deriving (Show)

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "dive"
  state <- newIORef $ State 0 0
  displayCallback $= display state
  keyboardMouseCallback $= Just (input state)
  mainLoop

nextState :: Key -> State -> State
nextState (SpecialKey KeyUp) (State x y) = State x (y + 1)
nextState (SpecialKey KeyDown) (State x y) = State x (y - 1)
nextState (SpecialKey KeyRight) (State x y) = State (x + 1) y
nextState (SpecialKey KeyLeft) (State x y) = State (x - 1) y
nextState _ state = state

input :: IORef State -> KeyboardMouseCallback
input state key keyState modifiers position = do
  state $~ nextState key
  x <- get state
  postRedisplay Nothing

display :: IORef State -> DisplayCallback
display state = do
  clear [ ColorBuffer ]
  State x y <- get state
  renderPrimitive Points $ do
    vertex $ Vertex3 (fromIntegral x / 100) (fromIntegral y / 100) (0 :: GLfloat)
  flush
