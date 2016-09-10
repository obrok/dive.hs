module Dive.Graphics.Window where

import Graphics.UI.GLFW as GLFW

init :: IO Window
init = do
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

  return window
