module Draw.SDL where

import qualified Graphics.UI.SDL as SDL
import Data.Bits

initialize :: IO ()
initialize = do
  0 <- SDL.init $ SDL.initFlagTimer .|. SDL.initFlagVideo
  return ()
