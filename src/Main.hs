module Main (main) where

import Draw

main :: IO ()
main = withCanvas (640, 480) $ \ctx -> do
  drawRect (50, 50) (300, 200) black ctx
  drawRect (75, 75) (300, 200) red ctx
  drawCircle (300, 300) 50 blue ctx
