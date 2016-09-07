import Graphics.GLUtil.VertexArrayObjects (makeVAO, withVAO)
import Graphics.GLUtil (simpleShaderProgram, bufferIndices, drawIndexedTris, program)
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL
import Data.IORef
import Game
import Paths_dive_hs (getDataFileName)
import Data.Vinyl
import Graphics.VinylGL
import Linear (V2(..))

data Drawable = Drawable Int Int

type Pos = '("vertexCoord", V2 GLfloat)

pos :: SField Pos
pos = SField

main :: IO ()
main = do
  True <- GLFW.init
  Just primary <- GLFW.getPrimaryMonitor
  Just VideoMode{videoModeHeight, videoModeWidth, videoModeRedBits, videoModeGreenBits, videoModeBlueBits, videoModeRefreshRate} <- GLFW.getVideoMode primary
  GLFW.windowHint (WindowHint'ContextVersionMajor 3)
  GLFW.windowHint (WindowHint'ContextVersionMinor 2)
  GLFW.windowHint (WindowHint'OpenGLProfile OpenGLProfile'Core)
  GLFW.windowHint (WindowHint'OpenGLForwardCompat True)
  GLFW.windowHint (WindowHint'RedBits videoModeRedBits)
  GLFW.windowHint (WindowHint'GreenBits videoModeGreenBits)
  GLFW.windowHint (WindowHint'BlueBits videoModeBlueBits)
  GLFW.windowHint (WindowHint'AlphaBits 8)
  GLFW.windowHint (WindowHint'RefreshRate videoModeRefreshRate)
  GLFW.windowHint (WindowHint'ClientAPI ClientAPI'OpenGL)
  Just window <- GLFW.createWindow videoModeWidth videoModeHeight "DIVE" (Just primary) Nothing
  GLFW.makeContextCurrent (Just window)
  GLFW.swapInterval 1

  -- path <- getDataFileName "res/dude.png"
  -- Right dudeTexture <- readTexture path
  -- print dudeTexture

  -- texture Texture2D $= Enabled
  -- activeTexture $= TextureUnit 0
  -- textureBinding Texture2D $= Just dudeTexture

  state <- newIORef initialState
  setKeyCallback window $ Just (input state)
  mainLoop window state

mainLoop :: Window -> IORef State -> IO ()
mainLoop window state = do
  display window state
  GLFW.waitEvents
  mainLoop window state

tileSize :: Int
tileSize = 24

input :: IORef State -> KeyCallback
input state _window key _someInt KeyState'Released _modifiers =
  state $~ updateState key
input _ _ _ _ _ _ = return ()

display :: Window -> IORef State -> IO ()
display window stateRef = do
  (x, y) <- GLFW.getWindowSize window
  let tiler = tile (x `div` tileSize) (y `div` tileSize)
  clear [ ColorBuffer ]
  state <- get stateRef
  render tiler state
  GLFW.swapBuffers window

render :: (Drawable -> [V2 GLfloat]) -> State -> IO ()
render tiler state = do
  vertices      <- bufferVertices . tileTex $ tiles
  vertexPath    <- getDataFileName "src/shaders/tile.vert"
  fragmentPath  <- getDataFileName "src/shaders/tile.frag"
  shaderProgram <- simpleShaderProgram vertexPath fragmentPath
  indexBuffer   <- bufferIndices indices
  vertexVAO     <- makeVAO $ do
    enableVertices' shaderProgram vertices
    bindVertices vertices
    bindBuffer ElementArrayBuffer $= Just indexBuffer
  currentProgram $= Just (program shaderProgram)
  withVAO vertexVAO $ drawIndexedTris (fromIntegral numVertices)
  where
    indices     = take numVertices $ foldMap (flip map [0,1,2,2,1,3] . (+)) [0,4..]
    numVertices = 6 * length tiles
    tiles       = concatMap tiler . drawables $ state

drawables :: State -> [Drawable]
drawables state = charDrawable : mobDrawables
  where charDrawable = Drawable characterX characterY
        Character characterX characterY = getCharacter state
        mobDrawables = map mobDrawable (getMobs state)
        mobDrawable (Mob x y _) = Drawable x y

tile :: Int -> Int -> Drawable -> [V2 GLfloat]
tile tilesX tilesY (Drawable x y) =
  V2 <$> [x1, x2] <*> [y1, y2]
  where x1 = 2 * (fromIntegral x / fromIntegral tilesX) - 1
        x2 = 2 * (fromIntegral (x + 1) / fromIntegral tilesX) - 1
        y1 = 2 * (fromIntegral y / fromIntegral tilesY) - 1
        y2 = 2 * (fromIntegral (y + 1) / fromIntegral tilesY) - 1

tileTex :: [V2 GLfloat] -> [FieldRec '[Pos]]
tileTex = map (pos =:)
