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
  GLFW.init >>= print
  GLFW.getVersion >>= print
  GLFW.windowHint (WindowHint'ContextVersionMajor 3)
  GLFW.windowHint (WindowHint'ContextVersionMinor 2)
  GLFW.windowHint (WindowHint'OpenGLDebugContext True)
  GLFW.windowHint (WindowHint'OpenGLProfile OpenGLProfile'Core)
  GLFW.windowHint (WindowHint'OpenGLForwardCompat True)
  Just primary <- GLFW.getPrimaryMonitor
  Just VideoMode{videoModeHeight, videoModeWidth} <- GLFW.getVideoMode primary
  Just window <- GLFW.createWindow videoModeWidth videoModeHeight "DIVE" (Just primary) Nothing
  GLFW.getWindowOpenGLProfile window >>= print
  GLFW.makeContextCurrent (Just window)

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
  get (shaderSourceBS shader) >>= print
  compileShader shader >>= print
  compileStatus shader >>= print
  shaderInfoLog shader >>= print

  state <- newIORef initialState
  setKeyCallback window $ Just (input state)
  mainLoop window state

mainLoop window state = do
  get state >>= print
  display window state
  GLFW.waitEvents
  mainLoop window state

tileSize :: Int
tileSize = 24

input :: IORef State -> KeyCallback
input state window key _someInt KeyState'Released _modifiers =
  state $~ updateState key
input _ _ _ _ _ _ = return ()

display :: Window -> IORef State -> IO ()
display window stateRef = do
  clear [ ColorBuffer ]
  state <- get stateRef
  (x, y) <- GLFW.getWindowSize window
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
green = Color3 0 0 1

quad :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
quad x1 x2 y1 y2 =
  renderPrimitive Quads $
    mapM_ (\[x, y] -> vertex $ Vertex2 x y) points
  where points = [[x1, y1], [x2, y1], [x2, y2], [x1, y2]]
