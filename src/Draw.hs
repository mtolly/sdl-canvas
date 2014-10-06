{-# LANGUAGE CPP #-}
module Draw
( module X
, module Draw.Util
, withCanvas
) where

#ifdef CANVAS
import Draw.Canvas as X
#else
import Draw.SDL as X
#endif
import Draw.Util

withCanvas :: Dims -> (Context -> IO a) -> IO a
withCanvas dims f = do
  initialize
  c <- newCanvas dims
  ctx <- getContext c
  clear ctx
  x <- f ctx
  finish
  return x
