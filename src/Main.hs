import Graphics.UI.GLUT
import Graphics.GLUtil.JuicyTextures (readTexture)
import Data.IORef
import Control.Monad
import Game
import Paths_dive_hs (getDataFileName)

data Drawable = Drawable Int Int (Color3 GLfloat)

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "dive"
  path <- getDataFileName "res/dude.png"
  aTexture <- readTexture path
  print aTexture
  fullScreen
  state <- newIORef initialState
  displayCallback $= display state
  keyboardMouseCallback $= Just (input state)
  mainLoop

tileSize :: GLsizei
tileSize = 24

input :: IORef State -> KeyboardMouseCallback
input state key Down _modifiers _position = do
  state $~ updateState key
  postRedisplay Nothing
input _ _ _ _ _ = return ()

display :: IORef State -> DisplayCallback
display stateRef = do
  clear [ ColorBuffer ]
  state <- get stateRef
  Size x y <- get windowSize
  preservingMatrix $ do
    ortho2D 0 (fromIntegral $ x `div` tileSize) 0 (fromIntegral $ y `div` tileSize)
    render state
  flush

render :: State -> IO ()
render state = forM_ (drawables state) draw

draw :: Drawable -> IO ()
draw (Drawable x y c) = do
  color c
  tile x y

drawables :: State -> [Drawable]
drawables state = charDrawable : mobDrawables
  where charDrawable = Drawable characterX characterY green
        Character characterX characterY = getCharacter state
        mobDrawables = map mobDrawable (getMobs state)
        mobDrawable (Mob x y _) = Drawable x y red

tile :: Int -> Int -> IO ()
tile x y =
  quad (fromIntegral x) (fromIntegral (x + 1)) (fromIntegral y) (fromIntegral (y + 1))

red :: Color3 GLfloat
red = Color3 1 0 0

green :: Color3 GLfloat
green = Color3 0 1 0

quad :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
quad x1 x2 y1 y2 =
  renderPrimitive Quads $
    mapM_ (\[x, y] -> vertex $ Vertex2 x y) points
  where points = [[x1, y1], [x2, y1], [x2, y2], [x1, y2]]
