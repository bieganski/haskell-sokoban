{-# LANGUAGE OverloadedStrings #-} 

import CodeWorld

main :: Program
type Program = IO ()


_obj, wall_pict, box_pict, ground_pict, storage_pict :: Picture
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

data Coord = C Integer Integer deriving Show

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

data Direction = R | U | L | D

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

pictureOfMaze = pictures ([atCoord (C x y) (drawTile (maze (C x y)))
    | x <- [-5 .. 5], y <- [-5 .. 5]])

pictureOfMazeAndPlayer :: Coord -> Picture -> Picture -> Picture
pictureOfMazeAndPlayer c pl mz = pictures([atCoord c pl] ++ [mz])

initialCoord :: Coord
initialCoord = C 1 (-2)


-------------------- for walk1 ---------

handleTime1 :: Double -> Coord -> Coord
handleTime1 _ c = c

handleEvent1 :: Event -> Coord -> Coord
handleEvent1 (KeyPress key) c
    | key == "Up" = if permittedPos (shiftCoords U c) then shiftCoords U c else c
    | key == "Down" = if permittedPos (shiftCoords D c) then shiftCoords D c else c
    | key == "Right" = if permittedPos (shiftCoords R c) then shiftCoords R c else c
    | key == "Left" = if permittedPos (shiftCoords L c) then shiftCoords L c else c
handleEvent1 _ c = c

drawState1 :: Coord -> Picture
drawState1 c = pictures([pictureOfMazeAndPlayer c player1 pictureOfMaze])

walk1 :: IO ()
walk1 = interactionOf initialCoord handleTime1 handleEvent1 drawState1


-------------------- for walk2 ---------

data World = W Coord Direction
initialWorld = W initialCoord L

handleTime :: Double -> World -> World
handleTime _ w = w

permittedPos :: Coord -> Bool 
permittedPos c = if maze c == Ground || maze c == Storage then True else False

handleEvent :: Event -> World -> World
handleEvent (KeyPress key) (W c d)
    | key == "Up" = if permittedPos (shiftCoords U c) then W (shiftCoords U c) U else W c d
    | key == "Down" = if permittedPos (shiftCoords D c) then W (shiftCoords D c) R else W c d
    | key == "Right" = if permittedPos (shiftCoords R c) then W (shiftCoords R c) D else W c d
    | key == "Left" = if permittedPos (shiftCoords L c) then W (shiftCoords L c) L else W c d
handleEvent _ w = w

drawState :: World -> Picture
drawState (W c d) = pictures([pictureOfMazeAndPlayer c (player2 d) pictureOfMaze])


walk2 :: IO ()
walk2 = interactionOf initialWorld handleTime handleEvent drawState


-------------------- for walk3 ---------

resettableInteractionOf ::
    World ->
    (Double -> World -> World) ->
    (Event -> World -> World) ->
    (World -> Picture) ->
    IO ()

resettableInteractionOf w t _ d = interactionOf w t enhancedHandleEvent d

enhancedHandleEvent :: Event -> World -> World
enhancedHandleEvent (KeyPress "Esc") _ = initialWorld
enhancedHandleEvent (KeyPress key) (W c d) = handleEvent (KeyPress key) (W c d)
enhancedHandleEvent _ w = w
    
walk3 :: IO()
walk3 = resettableInteractionOf initialWorld handleTime handleEvent drawState 

main = walk3
