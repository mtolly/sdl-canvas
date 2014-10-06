module Draw.Canvas
( initialize
, Canvas()
, newCanvas
, deleteCanvas
, finish
, Context()
, getContext
, clear
, drawRect
) where

import Draw.Util

import GHCJS.Types
-- import GHCJS.Foreign

initialize :: IO ()
initialize = return () -- wait for document.ready?

data Canvas_
type Canvas = JSRef Canvas_

foreign import javascript unsafe
  "js_newCanvas"
  js_newCanvas :: Int -> Int -> IO Canvas

newCanvas :: Dims -> IO Canvas
newCanvas (w, h) = js_newCanvas w h

foreign import javascript unsafe
  "$1.parentNode.removeChild($1);"
  deleteCanvas :: Canvas -> IO ()

finish :: IO ()
finish = return ()

data Context_
type Context = JSRef Context_

foreign import javascript unsafe
  "$1.getContext('2d')"
  getContext :: Canvas -> IO Context

foreign import javascript unsafe
  "$1.clearRect(0, 0, $1.canvas.width, $1.canvas.height);"
  clear :: Context -> IO ()

foreign import javascript unsafe
  "js_drawRect"
  js_drawRect
  :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Context -> IO ()

drawRect :: Posn -> Dims -> RGBA -> Context -> IO ()
drawRect (x, y) (w, h) (r, g, b, a) = js_drawRect x y w h r g b a
