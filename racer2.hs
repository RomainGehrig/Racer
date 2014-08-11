
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

bc :: Circuit
bc = ["----------"
     ,"-s      e-"
     ,"----------"]

infinitCircuit = row : infinitCircuit
    where row = ' ' : row

bp :: Player
bp = Player (1,1) (1,0) '*'

addVec :: Num a => Num b => (a,b) -> (a,b) -> (a,b)
(x1,y1) `addVec` (x2,y2) = (x1+x2, y1+y2)

subVec :: Num a => Num b => (a,b) -> (a,b) -> (a,b)
v1 `subVec` v2@(x,y) = addVec v1 (-x,-y)

safeDigitToInt :: Char -> Maybe Int
safeDigitToInt n
    | n `elem` ['0'..'9'] = Just (digitToInt n)
    | otherwise = Nothing
