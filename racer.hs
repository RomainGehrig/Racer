import Data.List
import Data.Maybe

type Vec2D = (Int, Int)

type Race = (Position, Velocity, Circuit) -- Starting Position & Velocity
type Position = Vec2D
type Velocity = Vec2D
type Circuit = [[Tile]]
type Tile = Char
type Move = Vec2D

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

validPos :: Circuit -> Position -> Bool
validPos c pos = (getTileType $ getTile c pos) == Road

possibleMoves :: Circuit -> Position -> [Move]
possibleMoves c pos = [ (x,y) | x <- [-1,0,1], y <- [-1,0,1], validMove pos (x,y)]
    where validMove pos (x,y) = (getTileType . getTile c . move pos) (x,y) == Road 

possibleVelocities :: Velocity -> Circuit -> Position -> [Velocity]
possibleVelocities vel c pos = map (addVec vel) (possibleMoves c nextPos)
    where nextPos = addVec vel pos

solveRace :: Race -> Maybe [Move]
solveRace r@(p, v, c) = Nothing

circuitFind :: Circuit -> Tile -> Maybe Position
circuitFind c t = listToMaybe [ (x,y) | (y, line) <- zip [0..] c, x <- elemIndices t line ]
