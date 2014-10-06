{-# LANGUAGE CPP #-}
module Draw
( module X
, module Draw.Util
) where

#ifdef CANVAS
import Draw.Canvas as X
#else
import Draw.SDL as X
#endif
import Draw.Util
