module Draw.SDL
( initialize
, Canvas()
, newCanvas
, deleteCanvas
, finish
, Context()
, getContext
, clear
, drawRect
, drawCircle
, drawPolygon
) where

import Draw.Util

import qualified Graphics.UI.SDL as SDL
import Data.Bits
import Foreign
import Foreign.C
import Control.Monad (unless)
import Control.Concurrent (threadDelay)

initialize :: IO ()
initialize = zero $ SDL.init $ SDL.initFlagTimer .|. SDL.initFlagVideo

type Canvas = SDL.Window

newCanvas :: Dims -> IO Canvas
newCanvas (w, h) = withCString "" $ \str ->
  notNull $ SDL.createWindow
    str
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
    Just (SDL.QuitEvent {}) -> SDL.quit
    _ -> threadDelay 1000 >> finish

type Context = SDL.Renderer

getContext :: Canvas -> IO Context
getContext window = notNull $
  SDL.createRenderer window (-1) SDL.rendererFlagAccelerated

clear :: Context -> IO ()
clear rend = do
  zero $ SDL.setRenderDrawColor rend 255 255 255 255
  zero $ SDL.renderClear rend
  SDL.renderPresent rend

drawRect :: Posn -> Dims -> RGBA -> Context -> IO ()
drawRect (x, y) (w, h) (r, g, b, a) rend = do
  zero $ SDL.setRenderDrawColor rend (fi r) (fi g) (fi b) (fi a)
  alloca $ \prect -> do
    poke prect $ SDL.Rect (fi x) (fi y) (fi w) (fi h)
    zero $ SDL.renderFillRect rend prect
  SDL.renderPresent rend

drawCircle :: Posn -> Int -> RGBA -> Context -> IO ()
drawCircle (x, y) rad (r, g, b, a) rend = do
  zero $
    c_filledCircleRGBA rend (fi x) (fi y) (fi rad) (fi r) (fi g) (fi b) (fi a)
  SDL.renderPresent rend

drawPolygon :: [Posn] -> RGBA -> Context -> IO ()
drawPolygon pns (r, g, b, a) rend = do
  let vx = map (fi . fst) pns
      vy = map (fi . snd) pns
  zero $ withArrayLen vx $ \len pvx ->
    withArray vy $ \pvy ->
      c_filledPolygonRGBA rend pvx pvy (fi len) (fi r) (fi g) (fi b) (fi a)
  SDL.renderPresent rend

-- SDL utils

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

notNull :: IO (Ptr a) -> IO (Ptr a)
notNull act = do
  p <- act
  if p == nullPtr
    then SDL.getError >>= peekCString >>= error
    else return p

zero :: (Eq a, Num a) => IO a -> IO ()
zero act = do
  n <- act
  unless (n == 0) $ SDL.getError >>= peekCString >>= error

pollEvent :: IO (Maybe SDL.Event)
pollEvent = alloca $ \pevt -> do
  e <- SDL.pollEvent pevt
  if e == 1
    then fmap Just $ peek pevt
    else return Nothing

foreign import ccall unsafe
  "filledCircleRGBA"
  c_filledCircleRGBA
  :: SDL.Renderer
  -> Int16 -> Int16 -> Int16
  -> Word8 -> Word8 -> Word8 -> Word8
  -> IO CInt

foreign import ccall unsafe
  "filledPolygonRGBA"
  c_filledPolygonRGBA
  :: SDL.Renderer
  -> Ptr Int16 -> Ptr Int16 -> CInt
  -> Word8 -> Word8 -> Word8 -> Word8
  -> IO CInt
