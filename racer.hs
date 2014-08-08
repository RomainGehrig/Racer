import Data.List
import Data.Maybe
import Data.Char

type Vec2D = (Int, Int)

type Race = (Position, Velocity, Circuit) -- Starting Position & Velocity
type Position = Vec2D
type Velocity = Vec2D
type Circuit = [[Tile]]
type Tile = Char
type Move = Vec2D
type Player = (Vec2D, Tile)

data TileType = Obstacle | Road deriving (Show, Eq) 
-- data Move = NW | N | NE |
--              W | C | E  |
--             SW | S | SE deriving (Show, Eq)

getTileType :: Tile -> TileType
getTileType t
    | t == ' '  = Road
    | otherwise = Obstacle

getTileTypeReverse :: TileType -> Tile
getTileTypeReverse t
    | t == Road     = ' '
    | t == Obstacle = '-'

getTile :: Circuit -> Position -> Tile
getTile c (x,y) 
    | x < 0 || y < 0 || x >= (length . head $ c) || y >= length c = getTileTypeReverse Obstacle
    | otherwise = c !! y !! x

basicCircuit :: Circuit
basicCircuit = ["------------",
                "-b        e-",
                "------------"]

basicRace :: Race
basicRace = ((1,1), (0,1), basicCircuit)

addVec :: Vec2D -> Vec2D -> Vec2D
(x1,y1) `addVec` (x2,y2) = (x1+x2, y1+y2)

subVec :: Vec2D -> Vec2D -> Vec2D
v1 `subVec` v2@(x,y) = addVec v1 (-x,-y)

move :: Position -> Velocity -> Position
move = addVec

possiblePos :: Position -> Velocity -> [Position]
possiblePos pos vel = [ addVec nextPos (dx,dy) | dy <- [-1..1], dx <- [-1..1]]
    where nextPos = addVec pos vel

validPos :: Circuit -> Position -> Bool
validPos c pos = (getTileType $ getTile c pos) == Road

-- possibleMoves :: Circuit -> Position -> [Move]
-- possibleMoves c pos = [ (x,y) | x <- [-1,0,1], y <- [-1,0,1], validMove pos (x,y)]
--     where validMove pos (x,y) = (getTileType . getTile c . move pos) (x,y) == Road 

-- possibleVelocities :: Velocity -> Circuit -> Position -> [Velocity]
-- possibleVelocities vel c pos = map (addVec vel) (possibleMoves c nextPos)
--     where nextPos = addVec vel pos

solveRace :: Race -> Maybe [Move]
solveRace r@(p, v, c) = Nothing

circuitFind :: Circuit -> Tile -> Maybe Position
circuitFind = (listToMaybe .) . circuitFinds

circuitFinds :: Circuit -> Tile -> [Position]
circuitFinds c t = [ (x,y) | (y, line) <- zip [0..] c, x <- elemIndices t line ]

-- TODO : make it less naive
displayRace :: Circuit -> [Player] -> String
displayRace c [] = printCircuit c
displayRace c (p:ps) = displayRace (showPlayer c p) ps

printCircuit :: Circuit -> String
printCircuit = unlines

replaceCircuit :: Circuit -> Position -> Tile -> Circuit
replaceCircuit c pos@(x,y) t = (take y c) ++ 
                               ((replaceCircuit' (c !! y) x t):[]) ++ 
                               (drop (y+1) c)

replaceCircuit' :: [Tile] -> Int -> Tile -> [Tile]
replaceCircuit' l@(x:xs) index tile
    | l == [] = []
    | index == 0 = tile : xs
    | otherwise  =    x : (replaceCircuit' xs (index-1) tile)

showPlayer :: Circuit -> Player -> Circuit
showPlayer c p@(pos,t) = replaceCircuit c pos t

showMoves :: Circuit -> [Position] -> Circuit
showMoves c poss = displayTiles c tiles
    where tiles = [(intToDigit n, pos) | (n, pos) <- zip [1..] poss, validPos c pos]
          displayTiles c ts = foldl (\ c (t,pos) -> replaceCircuit c pos t) c ts

showPlayerTurn :: Circuit -> Player -> Velocity -> Circuit
showPlayerTurn c p@(pos,_) v = showPlayer (showMoves c (possiblePos pos v)) p

main :: IO ()
main = do
    let players = ((1,1),'*'):[]
    putStr $ displayRace basicCircuit players

handlePlayerTurn :: Circuit -> Player -> Velocity -> Bool
handlePlayerTurn c p v = do
    pressedKey <- getChar
    case safeDigitToInt pressedKey of
        Just n -> putStrLn $ "Works "++(pressedKey:[])
        Nothing -> putStrLn "WRONG"
    where safeDigitToInt x = if x `elem` ['1'..'9'] then Just (digitToInt x) else Nothing

-- isRaceDone :: Circuit -> [Player] -> Maybe Bool
-- isRaceDone c ps = circuitFind c 'e' >>= \ endPos ->  Just $ any (\ p@(pos,_) -> pos == endPos) ps

isRaceDone :: Circuit -> [Player] -> Bool
isRaceDone c ps = case circuitFind c 'e' of
                     Nothing -> False
                     Just endPos -> any (\ p@(pos,_) -> pos == endPos) ps

bp :: Player
bp = ((1,1),'*')
bc :: Circuit
bc = basicCircuit
