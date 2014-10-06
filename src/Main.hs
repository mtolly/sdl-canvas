module Main (main) where

import Draw

main :: IO ()
main = do
  initialize
  c <- newCanvas 640 480
  ctx <- getContext c
  clear ctx
  drawRect (50, 50) (300, 200) black ctx
  drawRect (75, 75) (300, 200) red ctx
  finish
