module Dive.Graphics.Textures (Representation(..), loadTextures) where

import Graphics.GLUtil (texture2DWrap)
import Graphics.GLUtil.JuicyTextures
import Graphics.Rendering.OpenGL
import Paths_dive_hs (getDataFileName)
import System.FilePath

data Representation = Dude | Devil
  deriving (Eq, Ord, Enum, Show)

loadTextures :: IO [TextureObject]
loadTextures = mapM (fmap (either error id) . loadTexture) [(Dude)..]

loadTexture :: Representation -> IO (Either String TextureObject)
loadTexture representation = do 
  path <- getDataFileName $ "res" </> "tex" </> show representation ++ ".png"
  img  <- readTexture path
  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
  texture2DWrap           $= (Repeated, ClampToEdge)
  return img
