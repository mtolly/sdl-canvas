module Draw.Canvas where

import GHCJS.Types
-- import GHCJS.Foreign

initialize :: IO ()
initialize = return () -- wait for document.ready?

data Canvas_
type Canvas = JSRef Canvas_

foreign import javascript unsafe
  "js_newCanvas"
  newCanvas :: Int -> Int -> IO Canvas

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
  "js_blackRect"
  blackRect :: Int -> Int -> Int -> Int -> Context -> IO ()
