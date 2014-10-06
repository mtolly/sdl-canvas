module Main (main) where

import Draw

main :: IO ()
main = do
  initialize
  c <- newCanvas 640 480
  ctx <- getContext c
  clear ctx
  blackRect 50 50 300 200 ctx
  finish
