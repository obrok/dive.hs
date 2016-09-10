module Dive.Graphics.Textures (Representation(..), loadTextures) where

import Graphics.GLUtil (texture2DWrap)
import Graphics.GLUtil.JuicyTextures
import Graphics.Rendering.OpenGL
import Paths_dive_hs (getDataFileName)
import System.FilePath
import qualified Data.Map.Strict as Map

data Representation = Dude | Devil
  deriving (Eq, Ord, Enum, Show)

newtype Textures = Textures (Map.Map Representation TextureObject)

loadTextures :: IO (Representation -> TextureObject)
loadTextures = do
  textureObjects <- mapM (fmap (either error id) . loadTexture) [(Dude)..]
  let textures = Textures . Map.fromList . zip [(Dude)..] $ textureObjects
  return $ toTexture textures

loadTexture :: Representation -> IO (Either String TextureObject)
loadTexture representation = do 
  path <- getDataFileName $ "res" </> "tex" </> show representation ++ ".png"
  img  <- readTexture path
  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
  texture2DWrap           $= (Repeated, ClampToEdge)
  return img

toTexture :: Textures -> Representation -> TextureObject
toTexture (Textures ts) r = t
  where Just t = Map.lookup r ts
