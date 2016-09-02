import Graphics.UI.GLUT
import Data.IORef
import Control.Monad
import Game

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "dive"
  fullScreen
  state <- newIORef $ initialState
  displayCallback $= display state tileSize
  keyboardMouseCallback $= Just (input state)
  mainLoop

tileSize = 24

input :: IORef State -> KeyboardMouseCallback
input state key Down modifiers position = do
  state $~ updateState key
  postRedisplay Nothing
input _ _ _ _ _ = return ()

display :: IORef State -> GLsizei -> DisplayCallback
display stateRef tileSize = do
  clear [ ColorBuffer ]
  state <- get stateRef
  Size x y <- get windowSize
  preservingMatrix $ do
    ortho2D 0 (fromIntegral $ x `div` tileSize) 0 (fromIntegral $ y `div` tileSize)
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
  quad (fromIntegral x) (fromIntegral (x + 1)) (fromIntegral y) (fromIntegral (y + 1))

red = color3f 1 0 0

green = color3f 0 1 0

color3f r g b= color $ Color3 r g (b :: GLfloat)

quad :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
quad x1 x2 y1 y2 =
  renderPrimitive Quads $ do
    mapM_ (\[x, y] -> vertex $ Vertex2 x y) points
  where points = [[x1, y1], [x2, y1], [x2, y2], [x1, y2]]
