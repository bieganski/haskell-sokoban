{-# LANGUAGE OverloadedStrings #-} 

import CodeWorld
import qualified Data.Text as TXT

type Program = IO ()


_obj :: Picture
_obj = solidRectangle 1 1

wall_pict, box_pict, ground_pict, storage_pict, blank_pict :: Picture
wall_pict = colored red _obj
box_pict = colored brown _obj
ground_pict = colored (light brown) _obj
storage_pict = rectangle 1 1 & colored green (solidCircle 0.3)
blank_pict = rectangle 1 1


data Tile = Wall | Ground | Storage | Box | Blank deriving (Eq, Show)

drawTile :: Tile -> Picture
drawTile Wall    = wall_pict
drawTile Ground  = ground_pict
drawTile Storage = storage_pict
drawTile Box     = box_pict
drawTile Blank   = blank_pict

data Coord = C Integer Integer deriving (Eq, Show)

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

data Direction = R | U | L | D deriving (Eq, Show, Enum)

nextPos :: Coord -> Direction -> Coord
nextPos (C x y) U = C x (y + 1)
nextPos (C x y) D = C x (y - 1)
nextPos (C x y) R = C (x + 1) y
nextPos (C x y) L = C (x - 1) y


---------------------    ETAP 0    ---------------------

data Maze = Maze Coord (Coord -> Tile)

properMaze1 :: Maze
properMaze1 = Maze start fun where
  start = C 1 (-2)
  fun (C x y)
    | abs x > 4  || abs y > 4  = Blank
    | abs x == 4 || abs y == 4 = Wall
    | x ==  2 && y <= 0        = Wall
    | x ==  3 && y <= 0        = Storage
    | x >= -2 && y == 0        = Box
    | otherwise                = Ground


properMaze2 :: Maze
properMaze2 = Maze start fun where
  start = C (-3) 0
  fun (C x y)
   | elem x [(-1), 1, 3] && y == 0       = Box
   | x == 2 && elem y [1, 2, 3]          = Storage
   | abs x > 5  || y > 4 || y < (-3)     = Blank
   | x < 1 && y > 1                      = Blank
   | x > 3 && y < (-1)                   = Blank
   | x <= 1 && y >= 1                    = Wall
   | x >= 3 && y <= (-1)                 = Wall
   | x <= 1 && y >= 1                    = Wall
   | y == 4 || y == (-3)                 = Wall
   | abs x == 5                          = Wall
   | y == (-1) && elem x [(-3), (-1), 1] = Wall
   | x == 3 && elem y [1, 2]             = Wall
   | otherwise                           = Ground
    

badMaze1 :: Maze
badMaze1 = Maze start fun where
  start = C 1 (-2)
  fun (C x y)
    | abs x > 4  || abs y > 4  = Blank
    | abs x == 4 || abs y == 4 = Wall
    | x ==  2 && y <= 0        = Wall
    | x ==  3 && y <= 0        = Storage
    | x >= -2 && y == 0        = Box
    | x == 3 &&  y == 1        = Wall
    | otherwise                = Ground
    
    
badMaze2 :: Maze
badMaze2 = Maze start fun where
  start = C 5 5
  fun (C x y)
    | abs x > 3  || abs y > 3  = Blank
    | x <= 0 && y <= 0         = Blank
    | x <= 1 && y <= 1         = Wall
    | abs x == 3 || abs y == 3 = Wall
    | x == 2 && y == 0         = Storage
    | x == 2 && y == (-2)      = Box
    | otherwise                = Ground
    
    
mazes :: [Maze]
mazes = [properMaze1, properMaze2]

badMazes :: [Maze]
badMazes = [badMaze1, badMaze2]


---------------------    ETAP 1    ---------------------

elemList :: Eq a => a -> [a] -> Bool
elemList el lst = any (==True) (map (==el) lst)

appendList :: [a] -> [a] -> [a]
appendList l1 l2 = foldList (\el lst -> el:lst) l2 l1

listLength :: [a] -> Integer
listLength [] = 0
listLength (_:xs) = 1 + (listLength xs)

filterList :: (a -> Bool) -> [a] -> [a]
filterList fun lst = foldList (\x l -> if fun x then x:l else l) [] lst

-- indexed from 0
nth :: [a] -> Integer -> a
nth [] _ = error "out of range"
nth (x:xs) n 
  | n == 0 = x
  | otherwise = nth xs (n - 1)

mapList :: (a -> b) -> [a] -> [b]
mapList fun lst = foldList (\x l -> (fun x):l) [] lst

andList :: [Bool] -> Bool
andList lst = foldList (\x val -> x && val) True lst

allList :: (a -> Bool) -> [a] -> Bool
allList fun lst = andList (mapList fun lst)

foldList :: (a -> b -> b) -> b -> [a] -> b
foldList fun startWith (x:xs) = fun x (foldList fun startWith xs)
foldList _ startWith [] = startWith


---------------------    ETAP 2    ---------------------

reachableList :: Eq a => a -> (a -> [a]) -> [a]
reachableList initial neighs = reachList
  where
    reachList = search initial []

    search v visited = foldList (\next acc -> if elem next acc then acc else search next acc)
                           (v : visited)
                           (neighs v)


isGraphClosed :: Eq a => a -> (a -> [a]) -> (a -> Bool) -> Bool
isGraphClosed initial neighs isOk = allList isOk (reachableList initial neighs)


reachable :: Eq a => a -> a -> (a -> [a]) -> Bool
reachable v initial neighs = elemList v (reachableList initial neighs)


allReachable :: Eq a => [a] -> a -> (a -> [a]) -> Bool
allReachable vs initial neighs = allList (\v -> reachable v initial neighs) vs


---------------------    ETAP 4    ---------------------

steppableTiles :: [Tile]
steppableTiles = [Ground, Storage]

isSteppable :: Maze -> Coord -> Bool
isSteppable (Maze _ maze) c = elem (maze c) steppableTiles

neighbours :: Maze -> Coord -> [Coord]
neighbours m c = filter (\x -> isSteppable m x) [nextPos c dir | dir <- [R ..]]


isClosed :: Maze -> Bool
isClosed maze@(Maze c m) = isSteppable maze c
  && isGraphClosed c (neighbours maze) (\x -> m x /= Blank)


isSane :: Maze -> Bool
isSane maze = stors >= boxes where
   stors = length (storages (removeBoxes maze))
   boxes = length (initialBoxes maze)


pictureOfBools :: [Bool] -> Picture
pictureOfBools xs = translated (-fromIntegral k /2) (fromIntegral k) (go 0 xs)
  where n = length xs
        k = findK 0 -- k is the integer square of n
        findK i | i * i >= n = i
                | otherwise  = findK (i+1)
        go _ [] = blank
        go i (b:bs) =
          translated (fromIntegral (i `mod` k))
                     (-fromIntegral (i `div` k))
                     (pictureOfBool b)
          & go (i+1) bs

        pictureOfBool True =  colored green (solidCircle 0.4)
        pictureOfBool False = colored red   (solidCircle 0.4)


closedMazes :: Picture
closedMazes = pictureOfBools (mapList isClosed (badMazes ++ mazes))

saneMazes :: Picture
saneMazes = pictureOfBools (mapList isSane (badMazes ++ mazes))


etap4 :: Picture
etap4 = (translated 5 0 (lettering "Sane"))
      & (translated 5 0 saneMazes)
      & (translated (-5) 0 (lettering "Closed"))
      & (translated (-5) 0 closedMazes)
      
      
---------------------    ETAP 5    ---------------------


player1 :: Picture
player1 = colored (violet) (solidPolygon [(0.0, 0.5), (-0.4, -0.40), (0.4, -0.40)])

player2 :: Direction -> Picture
player2 D = rotated  pi player1
player2 R = rotated (3*pi/2) player1
player2 L = rotated (pi/2) player1
player2 U = player1


-- State :: <list of mazes> <actual coord> <actual direction> <boxes> <moves>
data State = S [Maze] Coord Direction [Coord] Integer

instance Eq State 
  where S mazes nr pos d boxes == S mazes' nr' pos' d' boxes'
          = length mazes == length mazes' && nr == nr' && d == d' 
          && boxes == boxes' && pos == pos'


listReachableObjects :: Tile -> Maze -> [Coord]
listReachableObjects t (Maze c m) = filter types reachList
  where types c = m c == t
        reachList = reachableList c (neighbours (Maze c m))
  

initialBoxes :: Maze -> [Coord]
initialBoxes (Maze c m) = [x | x <- reachList, m x == Box]
  where reachList = reachableList c (neighbours (removeBoxes (Maze c m)))


storages :: Maze -> [Coord]
storages m = listReachableObjects Storage m


initialState :: [Maze] -> State
initialState [] = S [] (C 0 0) L [] 0
initialState (m:mazes) = S (m:mazes) initCoord L (initialBoxes m) 0 where Maze initCoord _ = m


removeBoxes :: Maze -> Maze
removeBoxes (Maze c m) = (Maze c fun)
  where fun c = if m c == Box then Ground else m c  


addBoxes :: [Coord] -> Maze -> Maze
addBoxes boxes (Maze c m) = (Maze c m') 
  where m' c = if elem c boxes then Box else m c
        

actualMaze :: State -> Maze
actualMaze (S [] _ _ _ _) = error "'actualMaze' on empty maze list"
actualMaze (S (m:_) _ _ boxes _) = addBoxes boxes (removeBoxes m)


drawMaze :: Maze -> Picture
drawMaze (Maze _ m) = pictures ([atCoord (C x y) (drawTile (m (C x y)))
    | x <- [-7 .. 7], y <- [-7 .. 7]])


draw :: State -> Picture
draw (S [] _ _ _ _) = winScreen
draw state@(S _ (C x y) dir _ mov)
  | isWinning state = levelWinScreen mov
  | otherwise = pictures([translated (fromIntegral x) (fromIntegral y) (player2 dir)] 
    ++ [drawMaze (actualMaze state)])

permittedMove :: State -> Direction -> Bool
permittedMove state@(S _ pos _ boxes _) d
    | elem (nextPos pos d) boxes = elem (m (nextPos (nextPos pos d) d)) steppableTiles
    | elem (m (nextPos pos d)) steppableTiles = True
    | otherwise = False
    where (Maze _ m) = actualMaze state


makeMove :: State -> Direction -> State
makeMove state@(S mazes pos _ boxes movs) d
    | not (permittedMove state d) = (S mazes pos d boxes movs)
    | elem (nextPos pos d) boxes 
      = S mazes (nextPos pos d) d 
        ([el | el <- boxes, el /= (nextPos pos d)] ++ [(nextPos (nextPos pos d) d)]) (movs+1)
    | elem (m (nextPos pos d)) steppableTiles = S mazes (nextPos pos d) d boxes (movs+1)
    | otherwise = S mazes pos d boxes movs
    where (Maze _ m) = actualMaze state


isWinning :: State -> Bool
isWinning (S [] _ _ _ _) = True
isWinning (S (m:_) _ _ boxes _) = andList (mapList (==Storage) (mapList mm boxes)) 
  where Maze _ mm = m


handleEvent :: Event -> State -> State
handleEvent (KeyPress key) s
    | isWinning s = if key == " " then nextLevel s else s
    | key == "Up" = makeMove s U
    | key == "Down" = makeMove s D
    | key == "Right" = makeMove s R
    | key == "Left" = makeMove s L
    where nextLevel (S mzs _ _ _ _) = case mzs of
            [] -> (S [] (C 0 0) L [] 0)
            _:mms -> initialState mms
handleEvent _ s = s

handleTime :: Double -> world -> world
handleTime _ w = w


data GameState world = StartScreen | Running world deriving Eq

startScreen :: Picture
startScreen = pictures [scaled 3 3 (lettering "Sokoban!"), 
    translated 0 (-3) (lettering "naciśnij spację by zacząć grę!")]

winScreen :: Picture
winScreen = pictures [scaled 2 2 (lettering "Wygrałeś!"), 
    translated 0 (-3) (lettering "Gratulacje!")]


levelWinScreen :: Integer -> Picture
levelWinScreen n = pictures [_text]
  where _text = lettering (TXT.append ("Poziom ukończony, liczba ruchów: ") 
                (TXT.pack (show n)))


data Interaction world = Interaction
    world
    (Double -> world -> world)
    (Event -> world -> world)
    (world -> Picture)


resettable :: Interaction s -> Interaction s
resettable (Interaction w t_func e_func d_func)
  = Interaction w t_func e_func' d_func
    where e_func' (KeyPress "Esc") _ = w
          e_func' e s = e_func e s


withStartScreen :: Interaction s -> Interaction (GameState s)
withStartScreen (Interaction w t_func e_func d_func)
  = Interaction w' t_func' e_func' d_func'
  where
    w' = StartScreen

    t_func' _ StartScreen = StartScreen
    t_func' t (Running s) = Running (t_func t s)
    
    e_func' (KeyPress " ") StartScreen = Running w
    e_func' _ StartScreen = StartScreen
    e_func' e (Running s) = Running (e_func e s)

    d_func' StartScreen = startScreen
    d_func' (Running s) = d_func s


data WithUndo world = WithUndo world [world]

withUndo :: Eq a => Interaction a -> Interaction (WithUndo a)
withUndo (Interaction w t_func e_func d_func) 
  = Interaction w' t_func' e_func' d_func'
  where
    w' = WithUndo w []
    
    t_func' t (WithUndo w stack) = WithUndo (t_func t w) stack
    
    e_func' (KeyPress key) (WithUndo w stack) | key == "U" 
      = case stack of [] -> WithUndo w []
                      s':stack' -> WithUndo s' stack'
    e_func'    e (WithUndo w stack)
     | new == w   = WithUndo w stack
     | otherwise  = WithUndo new (w:stack)
     where new = e_func e w
     
    d_func' (WithUndo w _) = d_func w
        

runInteraction :: Interaction s -> Program
runInteraction (Interaction w t e d) = interactionOf w t e d


initInteraction :: Interaction State
initInteraction = Interaction (initialState mazes) handleTime handleEvent draw

etap5 :: IO()
etap5 = runInteraction (withUndo (resettable (withStartScreen initInteraction)))

main :: Program
main = etap5
-- main = drawingOf etap4

