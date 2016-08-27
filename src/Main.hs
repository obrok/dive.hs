import Graphics.UI.GLUT
import Data.IORef
import Control.Monad

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
display stateRef = do
  clear [ ColorBuffer ]
  state <- get stateRef
  render state
  flush

render (State x y) = do
  red
  quad (fromIntegral x / 100) (fromIntegral x / 100 + 0.05) (fromIntegral y / 100) (fromIntegral y / 100 + 0.05)

red = color3f 1 0 0

color3f r g b= color $ Color3 r g (b :: GLfloat)

quad :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
quad x1 x2 y1 y2 =
  renderPrimitive Polygon $ do
    mapM_ (\[x, y] -> vertex $ Vertex2 x y) points
  where points = [[x1, y1], [x2, y1], [x2, y2], [x1, y2]]
