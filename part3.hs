{-# LANGUAGE OverloadedStrings #-} 

import CodeWorld

main :: Program
type Program = IO ()


_obj, wall_pict, box_pict, ground_pict, storage_pict, blank_pict :: Picture
_obj = solidRectangle 1 1
wall_pict = colored red _obj
box_pict = colored brown _obj
ground_pict = colored (light brown) _obj
storage_pict = rectangle 1 1 & colored green (solidCircle 0.3)
blank_pict = rectangle 1 1


data Tile = Wall | Ground | Storage | Box | Blank deriving Eq

drawTile :: Tile -> Picture
drawTile Wall    = wall_pict
drawTile Ground  = ground_pict
drawTile Storage = storage_pict
drawTile Box     = box_pict
drawTile Blank   = blank_pict

data Coord = C Integer Integer deriving (Show, Eq)

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

data Direction = R | U | L | D deriving Show

shiftCoords :: Direction -> Coord -> Coord
shiftCoords U (C x y) = C x (y+1)
shiftCoords D (C x y) = C x (y-1)
shiftCoords R (C x y) = C (x+1) y
shiftCoords L (C x y) = C (x-1) y

moveCoords :: [Direction] -> Coord -> Coord
moveCoords (el:list) (C x y) = shiftCoords el cc where cc = moveCoords list (C x y)
moveCoords _ c = c

maze :: Coord -> Tile
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

player1 :: Picture
player1 = colored (violet) (solidPolygon [(0.0, 0.5), (-0.4, -0.40), (0.4, -0.40)])

player2 :: Direction -> Picture
player2 R = rotated  pi player1
player2 D = rotated (3*pi/2) player1
player2 L = rotated (pi/2) player1
player2 U = player1

data State = S Coord Direction [Coord] deriving Show

boxes :: [Coord]
boxes = [(C x y) | x <- [-5..5], y <- [-5..5], maze(C x y) == Box]

initialState :: State
initialState = S (C 1 (-2)) L boxes 

type Maze = Coord -> Tile

removeBoxes :: Maze -> Coord -> Tile
removeBoxes m = f . m where f = \t -> if t == Box then Ground else t

addBoxes :: [Coord] -> Maze -> Maze
addBoxes l m c = (f . m) c where f = if elem c l then \t -> Box else id


drawMaze :: Maze -> Picture
drawMaze m = pictures ([atCoord (C x y) (drawTile (m (C x y)))
    | x <- [-5 .. 5], y <- [-5 .. 5]])


draw :: State -> Picture
draw (S c d cc) = pictures([drawMaze (addBoxes boxes (removeBoxes maze))] ++ [player2 d])


handleEvent :: Event -> State -> State
handleEvent (KeyPress key) c
    | key == "Up" = if permittedPos (shiftCoords U c) then shiftCoords U c else c
    | key == "Down" = if permittedPos (shiftCoords D c) then shiftCoords D c else c
    | key == "Right" = if permittedPos (shiftCoords R c) then shiftCoords R c else c
    | key == "Left" = if permittedPos (shiftCoords L c) then shiftCoords L c else c
handleEvent1 _ c = c



main = drawingOf (draw initialState)


