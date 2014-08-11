import Data.List
import Data.Maybe
import Data.Char

data Race = Race { startingVelocity :: Velocity 
                 , circuit :: Circuit } deriving Show

type Vec2D = (Int,Int)
type Position = Vec2D
type Velocity = Vec2D
type Circuit = [[Tile]]
type Tile = Char
type Move = Vec2D

data Player = Player { position :: Vec2D
                     , velocity :: Vec2D
                     , representation :: Tile } deriving Show

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
basicRace = Race {startingVelocity=(0,1), circuit=basicCircuit}

addVec :: Vec2D -> Vec2D -> Vec2D
(x1,y1) `addVec` (x2,y2) = (x1+x2, y1+y2)

subVec :: Vec2D -> Vec2D -> Vec2D
v1 `subVec` v2@(x,y) = addVec v1 (-x,-y)

move :: Position -> Velocity -> Position
move = addVec

possiblePos :: Player -> [Position]
possiblePos p = [ addVec nextPos (dx,dy) | dy <- [-1..1], dx <- [-1..1] ]
    where nextPos = addVec (position p) (velocity p)

validPos :: Circuit -> Position -> Bool
validPos c pos = (getTileType $ getTile c pos) == Road

-- possibleMoves :: Circuit -> Position -> [Move]
-- possibleMoves c pos = [ (x,y) | x <- [-1,0,1], y <- [-1,0,1], validMove pos (x,y)]
--     where validMove pos (x,y) = (getTileType . getTile c . move pos) (x,y) == Road 

-- possibleVelocities :: Velocity -> Circuit -> Position -> [Velocity]
-- possibleVelocities vel c pos = map (addVec vel) (possibleMoves c nextPos)
--     where nextPos = addVec vel pos

solveRace :: Race -> Maybe [Move]
solveRace r = Nothing

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
showPlayer c p = replaceCircuit c (position p) (representation p)

showMoves :: Circuit -> [Position] -> Circuit
showMoves c ps = displayNumberedTiles c (numberedTiles c ps)
    where displayNumberedTiles c ts = foldl (\ c (n,pos) -> replaceCircuit c pos (intToDigit n)) c ts

numberedTiles :: Circuit -> [Position] -> [(Int,Position)]
numberedTiles c ps = [(n, pos) | (n, pos) <- zip [1..] ps, validPos c pos]

showPlayerTurn :: Circuit -> Player -> Circuit
showPlayerTurn c p = showPlayer (showMoves c (possiblePos p)) p

main :: IO ()
main = do
    let players = bp:[]
    putStr $ displayRace bc players
    putStr . printCircuit $ showPlayerTurn bc bp

--    while (not . isRaceDone bc) handlePlayerTurn 

while :: (a -> Bool) -> (a -> a) -> a -> a
while p f x = go
    where go = if (p x') then while p f x' else x
          x' = f x

handlePlayerTurn :: Circuit -> Player -> Char -> Either Player Player
handlePlayerTurn c p key = case safeDigitToInt key of
                               Nothing -> Left p -- The player doesn't change
                               Just n -> case getMove' n of 
                                             Just m -> Right (movePlayerTo p m)
                                             Nothing -> Left p -- can't move there
    where safeDigitToInt x = if x `elem` ['1'..'9'] then Just (digitToInt x) else Nothing
          getMove n = case find ((==) n . fst) possibleMoves of
                          Nothing -> Nothing
                          Just (i,p) -> Just p
          getMove' n = find ((==) n . fst) possibleMoves >>= Just . snd
          possibleMoves = numberedTiles c (possiblePos p)
          
movePlayerTo :: Player -> Velocity -> Player
movePlayerTo p v = Player ((position p) `addVec` v) v (representation p)

-- isRaceDone :: Circuit -> [Player] -> Maybe Bool
-- isRaceDone c ps = circuitFind c 'e' >>= \ endPos ->  Just $ any (\ p@(pos,_) -> pos == endPos) ps

isRaceDone :: Circuit -> [Player] -> Bool
isRaceDone c ps = case circuitFind c 'e' of
                     Nothing -> False
                     Just endPos -> any (\ p -> (position p) == endPos) ps

bp :: Player
bp = (Player (1,1) (1,0) '*')
bc :: Circuit
bc = basicCircuit
