{-# LANGUAGE CPP #-}
module Draw
( module X
) where

#ifdef CANVAS
import Draw.Canvas as X
#else
import Draw.SDL as X
#endif
