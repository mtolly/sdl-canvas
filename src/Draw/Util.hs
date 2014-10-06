module Draw.Util where

type Posn = (Int, Int)
type Dims = (Int, Int)
type RGBA = (Int, Int, Int, Int)

black :: RGBA
black = (0, 0, 0, 255)

red :: RGBA
red = (255, 0, 0, 255)

blue :: RGBA
blue = (0, 0, 255, 255)
