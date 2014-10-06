module Draw.SDL where

import qualified Graphics.UI.SDL as SDL
import Data.Bits
import Foreign
import Foreign.C

import Control.Concurrent (threadDelay)

initialize :: IO ()
initialize = do
  0 <- SDL.init $ SDL.initFlagTimer .|. SDL.initFlagVideo
  return ()

type Canvas = SDL.Window

newCanvas :: Int -> Int -> IO Canvas
newCanvas w h = nullError $ SDL.createWindow
  nullPtr
  SDL.windowPosUndefined
  SDL.windowPosUndefined
  (fromIntegral w)
  (fromIntegral h)
  0

deleteCanvas :: Canvas -> IO ()
deleteCanvas = SDL.destroyWindow

finish :: IO ()
finish = do
  e <- pollEvent
  case e of
    Just (SDL.QuitEvent {}) -> return ()
    _ -> threadDelay 1000 >> finish

type Context = SDL.Renderer

getContext :: Canvas -> IO Context
getContext window = nullError $
  SDL.createRenderer window (-1) SDL.rendererFlagAccelerated

clear :: Context -> IO ()
clear ctx = do
  0 <- SDL.setRenderDrawColor ctx 255 255 255 255
  0 <- SDL.renderClear ctx
  SDL.renderPresent ctx

blackRect :: Int -> Int -> Int -> Int -> Context -> IO ()
blackRect x y w h ctx = do
  0 <- SDL.setRenderDrawColor ctx 0 0 0 255
  renderFillRect ctx $ SDL.Rect
    (fromIntegral x)
    (fromIntegral y)
    (fromIntegral w)
    (fromIntegral h)
  SDL.renderPresent ctx

-- SDL utils

nullError :: IO (Ptr a) -> IO (Ptr a)
nullError act = do
  p <- act
  if p == nullPtr
    then do
      err <- SDL.getError
      str <- peekCString err
      error str
    else return p

pollEvent :: IO (Maybe SDL.Event)
pollEvent = alloca $ \pevt -> do
  e <- SDL.pollEvent pevt
  if e == 1
    then fmap Just $ peek pevt
    else return Nothing

renderFillRect :: SDL.Renderer -> SDL.Rect -> IO ()
renderFillRect rend rect = alloca $ \prect -> do
  poke prect rect
  0 <- SDL.renderFillRect rend prect
  return ()
