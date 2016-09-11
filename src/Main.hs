import Control.Monad
import Data.IORef
import Data.Vinyl
import Dive.Graphics.Textures
import GHC.Exts
import Game
import Graphics.GLUtil (simpleShaderProgram, bufferIndices, drawIndexedTris, program, withTextures2D)
import Graphics.GLUtil.VertexArrayObjects (makeVAO, withVAO)
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import Graphics.VinylGL
import Linear (V2(..))
import Paths_dive_hs (getDataFileName)
import qualified Dive.Graphics.Window as Window

data Drawable = Drawable Int Int Representation

representation :: Drawable -> Representation
representation (Drawable _ _ r) = r

type Pos = '("vertexCoord", V2 GLfloat)
type Tex = '("texCoord", V2 GLfloat)

pos :: SField Pos
pos = SField

tex :: SField Tex
tex = SField

main :: IO ()
main = do
  window <- Window.init

  (x, y) <- GLFW.getWindowSize window
  let tiler = tile (x `div` tileSize) (y `div` tileSize)
  stateRenderer <- render tiler

  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

  state <- newIORef initialState
  setKeyCallback window $ Just (input state)
  mainLoop window state stateRenderer

mainLoop :: Window -> IORef State -> (State -> IO ()) -> IO ()
mainLoop window state stateRenderer = do
  display window state stateRenderer
  GLFW.waitEvents
  mainLoop window state stateRenderer

tileSize :: Int
tileSize = 24

input :: IORef State -> KeyCallback
input state _window key _someInt KeyState'Released _modifiers =
  state $~ updateState key
input _ _ _ _ _ _ = return ()

display :: Window -> IORef State -> (State -> IO ()) -> IO ()
display window stateRef stateRenderer = do
  clearColor $= lightGray
  clear [ ColorBuffer ]
  state <- get stateRef
  stateRenderer state
  GLFW.swapBuffers window

render :: (Drawable -> [V2 GLfloat]) -> IO (State -> IO ())
render tiler = do
  pickTexture   <- loadTextures
  vertexPath    <- getDataFileName "src/shaders/tile.vert"
  fragmentPath  <- getDataFileName "src/shaders/tile.frag"
  shaderProgram <- simpleShaderProgram vertexPath fragmentPath
  return $ \state ->
    forM_ (groupWith representation . drawables $ state) $ \ds@(d:_) -> do
      let indices     = take numVertices $ foldMap (flip map [0,1,2,2,1,3] . (+)) [0,4..]
          tiles       = map tiler ds
          numVertices = 6 * length tiles
      vertices    <- bufferVertices . tileTex $ tiles
      indexBuffer <- bufferIndices indices
      vertexVAO   <- makeVAO $ do
        enableVertices' shaderProgram vertices
        bindVertices vertices
        bindBuffer ElementArrayBuffer $= Just indexBuffer
      currentProgram $= Just (program shaderProgram)
      withVAO vertexVAO . withTextures2D [pickTexture $ representation d] $ drawIndexedTris (fromIntegral numVertices)

drawables :: State -> [Drawable]
drawables state = charDrawable : mobDrawables ++ wallDrawables
  where charDrawable = Drawable characterX characterY Dude
        Character characterX characterY = getCharacter state
        mobDrawables = map mobDrawable (getMobs state)
        mobDrawable (Mob x y _) = Drawable x y Devil
        wallDrawables = map wallDrawable (getWalls state)
        wallDrawable (x, y) = Drawable x y Wall

tile :: Int -> Int -> Drawable -> [V2 GLfloat]
tile tilesX tilesY (Drawable x y _) =
  V2 <$> [x1, x2] <*> [y1, y2]
  where x1 = 2 * (fromIntegral x / fromIntegral tilesX) - 1
        x2 = 2 * (fromIntegral (x + 1) / fromIntegral tilesX) - 1
        y1 = 2 * (fromIntegral y / fromIntegral tilesY) - 1
        y2 = 2 * (fromIntegral (y + 1) / fromIntegral tilesY) - 1

tileTex :: [[V2 GLfloat]] -> [FieldRec [Pos,Tex]]
tileTex = foldMap (flip (zipWith (<+>)) (cycle coords) . map (pos =:))
  where coords = map (tex =:) $ V2 <$> [0,1] <*> [1,0]

lightGray :: Color4 GLfloat
lightGray = Color4 0.2 0.2 0.2 1
