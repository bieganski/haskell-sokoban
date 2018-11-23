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

data Coord = C Integer Integer deriving (Eq, Show)

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

data Direction = R | U | L | D deriving (Eq, Show)

nextPos :: Coord -> Direction -> Coord
nextPos (C x y) U = C x (y + 1)
nextPos (C x y) D = C x (y - 1)
nextPos (C x y) R = C (x + 1) y
nextPos (C x y) L = C (x - 1) y

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

data State = S Coord Direction [Coord] 

boxes :: [Coord]
init_boxes = [(C x y) | x <- [-5..5], y <- [-5..5], maze(C x y) == Box]
boxes = [(C x y) | x <- [-5..5], y <- [-5..5], maze(C x y) == Box]
-- TODO 
initialState :: State
initialState = S (C 1 (-2)) L init_boxes

type Maze = Coord -> Tile

removeBoxes :: Maze -> Coord -> Tile
removeBoxes m = f . m where f = \t -> if t == Box then Ground else t

addBoxes :: [Coord] -> Maze -> Maze
addBoxes l m c = (f . m) c where f = if elem c l then \t -> Box else id


drawMaze :: Maze -> Picture
drawMaze m = pictures ([atCoord (C x y) (drawTile (m (C x y)))
    | x <- [-5 .. 5], y <- [-5 .. 5]])


draw :: State -> Picture
draw (S (C x y) d cc) = pictures([translated (fromIntegral x) (fromIntegral y) (player2 d)] ++ [drawMaze (addBoxes boxes (removeBoxes maze))])


permittedMove :: State -> Direction -> Bool
permittedMove (S c pl_d cc) d
    | elem (nextPos c d) cc = if elem (maze (nextPos (nextPos c d) d)) [Storage, Ground] then True else False
    | elem (maze (nextPos c d)) [Storage, Ground] = True
    | otherwise = False


makeMove :: State -> Direction -> State
makeMove (S c pl_d cc) d
    | not (permittedMove (S c pl_d cc) d) = S c d cc
    | elem (nextPos c d) cc = S (nextPos c d) d ([el | el <- cc, el /= (nextPos c d)] ++ [(nextPos (nextPos c d) d)])
    | elem (maze (nextPos c d)) [Storage, Ground] = S (nextPos c d) d cc
    | otherwise = S c d cc


handleEvent :: Event -> State -> State
handleEvent (KeyPress key) s
    | key == "Up" = makeMove s U
    | key == "Down" = makeMove s D
    | key == "Right" = makeMove s R
    | key == "Left" = makeMove s L
handleEvent _ s = s


handleTime :: Double -> world -> world
handleTime _ w = w


main = interactionOf initialState handleTime handleEvent draw


