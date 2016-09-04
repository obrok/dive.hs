{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module DisplayMode where
import qualified Language.C.Inline as C

C.include "<GLUT/glut.h>"

setDisplayMode :: IO ()
setDisplayMode = do
 res <- [C.exp| void{ glutInitDisplayMode(GLUT_3_2_CORE_PROFILE) } |]
 print res
