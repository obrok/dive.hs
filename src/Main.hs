import Graphics.UI.GLUT
import Data.IORef
import Control.Monad
import Game

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "dive"
  state <- newIORef $ initialState
  displayCallback $= display state
  keyboardMouseCallback $= Just (input state)
  mainLoop

input :: IORef State -> KeyboardMouseCallback
input state key Up modifiers position = do
  state $~ updateState key
  x <- get state
  postRedisplay Nothing
input _ _ _ _ _ = return ()

display :: IORef State -> DisplayCallback
display stateRef = do
  clear [ ColorBuffer ]
  state <- get stateRef
  render state
  flush

render (State (Character x y) (Mobs mobs)) = do
  red
  creature x y
  forM_ mobs renderMob

renderMob (Mob x y Alive) = do
  green
  creature x y
renderMob (Mob x y Dead) = return ()

creature x y =
  quad (fromIntegral x / 100) (fromIntegral (x + 1) / 100) (fromIntegral y / 100) (fromIntegral (y + 1) / 100)

red = color3f 1 0 0

green = color3f 0 1 0

color3f r g b= color $ Color3 r g (b :: GLfloat)

quad :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
quad x1 x2 y1 y2 =
  renderPrimitive Polygon $ do
    mapM_ (\[x, y] -> vertex $ Vertex2 x y) points
  where points = [[x1, y1], [x2, y1], [x2, y2], [x1, y2]]
