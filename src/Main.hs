import Graphics.GLUtil.JuicyTextures (readTexture)
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL
import Data.IORef
import Control.Monad
import Game
import Paths_dive_hs (getDataFileName)

data Drawable = Drawable Int Int (Color3 GLfloat)

main :: IO ()
main = do
  result <- GLFW.initialize
  VideoMode{videoWidth, videoHeight} <- get desktopMode
  version <- get GLFW.version
  print version
  print result
  window <- GLFW.openWindow (Size (fromIntegral videoWidth) (fromIntegral videoHeight)) [GLFW.DisplayAlphaBits 8] GLFW.Window
  print window

  path <- getDataFileName "res/dude.png"
  Right dudeTexture <- readTexture path
  print dudeTexture

  texture Texture2D $= Enabled
  activeTexture $= TextureUnit 0
  textureBinding Texture2D $= Just dudeTexture

  shaderPath <- getDataFileName "src/shaders/texture_quad.frag"
  contents <- readFile shaderPath
  print (packUtf8 contents)
  shader <- createShader FragmentShader
  shaderSourceBS shader $= packUtf8 contents
  compileShader shader
  status <- compileStatus shader
  infoLog <- shaderInfoLog shader
  print infoLog
  print status
  print shader

  -- state <- newIORef initialState
  -- displayCallback $= display state
  -- keyboardMouseCallback $= Just (input state)
  -- mainLoop

tileSize :: GLsizei
tileSize = 24

-- input :: IORef State -> KeyboardMouseCallback
-- input state key Down _modifiers _position = do
--   state $~ updateState key
--   postRedisplay Nothing
-- input _ _ _ _ _ = return ()

-- display :: IORef State -> DisplayCallback
-- display stateRef = do
--   clear [ ColorBuffer ]
--   state <- get stateRef
--   Size x y <- get windowSize
--   preservingMatrix $ do
--     ortho2D 0 (fromIntegral $ x `div` tileSize) 0 (fromIntegral $ y `div` tileSize)
--     render state
--   flush

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
green = Color3 0 0 1

quad :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
quad x1 x2 y1 y2 =
  renderPrimitive Quads $
    mapM_ (\[x, y] -> vertex $ Vertex2 x y) points
  where points = [[x1, y1], [x2, y1], [x2, y2], [x1, y2]]
