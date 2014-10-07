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
, drawCircle
, drawPolygon
) where

import Draw.Util

import GHCJS.Types
import GHCJS.Marshal

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

foreign import javascript unsafe
  "js_drawCircle"
  js_drawCircle
  :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Context -> IO ()

drawCircle :: Posn -> Int -> RGBA -> Context -> IO ()
drawCircle (x, y) rad (r, g, b, a) = js_drawCircle x y rad r g b a

foreign import javascript unsafe
  "js_drawPolygon"
  js_drawPolygon
  :: JSRef [[Int]] -> Int -> Int -> Int -> Int -> Context -> IO ()

drawPolygon :: [Posn] -> RGBA -> Context -> IO ()
drawPolygon pns (r, g, b, a) ctx = do
  pns' <- toJSRef [ [x, y] | (x, y) <- pns ]
  js_drawPolygon pns' r g b a ctx
