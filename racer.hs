import Data.List
import Data.Maybe
import Data.Char
import System.IO
import System.Process
import System.Directory
import System.FilePath

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
    | t /= '-'  = Road
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

cir1 :: Circuit
cir1 = ["----------------------------------------------------",
    "----------------------------------------------------",
    "--------------------------     ---------------------",
    "-------------------------         ------------------",
    "---------b---------------                  ---------",
    "------     --------------   --               -------",
    "------      -------------   ---               ------",
    "-------      ------------   ----    -------    -----",
    "-------       ------------  ----------------    ----",
    "--------            ------   ---------------    ----",
    "---------             ----   --------------    -----",
    "----------              --    ------------    ------",
    "------------     --           -----------     ------",
    "---------------------         ---------      -------",
    "----------------------      ----------     ---------",
    "-------------------------------------     ----------",
    "-------------    ------------    ----     ----------",
    "-----------       ---------       ----       -------",
    "---------         --------         -----       -----",
    "--------           ------           ------       ---",
    "-------        -    -----     --     -------      --",
    "-------      ----   ----     ----     --------     -",
    "-------     ------   ---     -----     -------     -",
    "------     -------    --    -------     ------     -",
    "------     --------         -------                -",
    "------     ---------        --------               -",
    "------     ----------      ----------             --",
    "------     -----------    -----------            ---",
    "------      ---------------------------        -----",
    "------       ---------------------------------------",
    "-------        -------------------------------------",
    "-------          ----------------- -----------------",
    "--------          ---------------   ----------------",
    "---------          --------------   --------  ------",
    "-------------       ------------    --------  ------",
    "----------------     ----------     --------   -----",
    "-----------------    ----------      ------    -----",
    "----------------     ---------       ------    -----",
    "--------------      ----------   -   -----      ----",
    "-----------        ----------    -    ----      ----",
    "--------          ----------     -    ---        ---",
    "------          ------------    ---   ---        ---",
    "------        ---------        ----    --    -   ---",
    "------       ------            ----    -     -    --",
    "------                        -----          --   --",
    "------                      -------          ---  --",
    "-------                ------------          --- e--",
    "--------         -------------------         -------",
    "------------------------------------        --------",
    "--------------------------------------      --------"]
    
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
replaceCircuit c (x,y) t = (take y c) ++ 
                               ((replaceCircuit' (c !! y) x t):[]) ++ 
                               (drop (y+1) c)

replaceCircuit' :: [Tile] -> Int -> Tile -> [Tile]
replaceCircuit' l@(x:xs) index tile
    | l == []    =   []
    | index == 0 = tile : xs
    | otherwise  =    x : (replaceCircuit' xs (index-1) tile)


multReplaceCircuit :: Circuit -> [Position] -> Tile -> Circuit
multReplaceCircuit c ps t = foldr (\pos nc -> replaceCircuit nc pos t) c ps

showPlayer :: Circuit -> Player -> Circuit
showPlayer c p = replaceCircuit c (position p) (representation p)

showMoves :: Circuit -> [Position] -> Circuit
showMoves c ps = displayNumberedTiles (numberedTiles c ps)
    where displayNumberedTiles ts = foldl (\ c1 (n,pos) -> replaceCircuit c1 pos (intToDigit n)) c ts

numberedTiles :: Circuit -> [Position] -> [(Int,Position)]
numberedTiles c ps = [(n, pos) | (n, pos) <- zip [1..] ps, validPos c pos]

showPlayerTurn :: Circuit -> Player -> Circuit
showPlayerTurn c p = showMoves (showPlayer c p) (possiblePos p)

-- raceList :: IO [FilePath]
-- raceList = do
--     races <- getDirectoryContents races_dir
--     return $ filter (isSuffixOf ".txt") races
--     where races_dir = "races"
          

playerTurn :: Circuit -> Player -> [Player] -> IO()
playerTurn c p ps
    | isRaceDone c (p:[]) = putStrLn $ "You won in " ++ (show . length $ ps) ++ " turns."
    | otherwise = do
                     putStr . printCircuit $  multReplaceCircuit (showPlayerTurn c p) (map position ps) (representation p)
                     k <- getChar
                     _ <- system "clear"
                     case handlePlayerTurn c p k of 
                        Left player -> playerTurn c oldPlayer (tail ps)
                        Right player -> playerTurn c player (player:ps)   
                     where oldPlayer = head . tail $ ps

-- Returns Right b if b is Just b, else return Left a
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither = (flip maybe Right) . Left

handlePlayerTurn :: Circuit -> Player -> Char -> Either Player Player
handlePlayerTurn c p key = maybeToEither p newPlayer
    where newPlayer = safeDigitToInt key >>= getMove >>= Just . movePlayerTo p
          safeDigitToInt x = if x `elem` ['1'..'9'] then Just (digitToInt x) else Nothing
          getMove n = find ((==) n . fst) possibleMoves >>= Just . snd
          possibleMoves = numberedTiles c (possiblePos p)
          
movePlayerTo :: Player -> Position -> Player
movePlayerTo p pos = Player pos newVel (representation p)
    where newVel = pos `subVec` (position p)

isRaceDone :: Circuit -> [Player] -> Bool
isRaceDone c ps = maybe False (const True) (listToMaybe [ p | p <- ps, end <- circuitFinds c 'e', (position p) == end])
                  -- maybe False [] (circuitFinds c 'e')
                  -- case circuitFind c 'e' of
                  --   Nothing -> False
                  --   Just endPos -> any (\ p -> (position p) == endPos) ps

playCircuit :: Circuit -> IO ()
playCircuit c = do
    let p = Player (maybe (0,0) id (circuitFind c 'b')) (0,0) '*'
    playerTurn c p []

selectFromList :: Show a => String -> [a] -> IO a
selectFromList message xs = do
    putStrLn message
    putStr $ unlines numeredItems
    selection <- scanLine ""
    case validInput selection of
        True -> return $ xs !! ((read selection :: Int) - 1)
        False -> selectFromList message xs
    where numeredItems = zipWith ((++) . (++". ")) nums items
          items = map show xs
          nums = map show [1..]
          max = length numeredItems 
          validInput s = any (==s) (take max nums)
          scanLine :: String -> IO String
          scanLine input = do
                        c <- hGetChar stdin
                        case c of
                            '\n' -> return $ reverse input
                            _ -> do
                               putChar c
                               scanLine (c : input)
          

availableCircuits :: FilePath -> IO [FilePath]
availableCircuits path = fmap fileFilter files
    where files = getDirectoryContents path
          fileFilter = filter (isSuffixOf ".txt")

getCircuit :: FilePath -> IO (Maybe Circuit)
getCircuit file = do
    exists <- doesFileExist file
    case exists of
        True -> fmap (Just . lines) (readFile file)
        False -> return Nothing

main :: IO ()
main = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    let directory = "races"
    circuits <- availableCircuits directory
    selectedCircuit <- selectFromList "What circuit would you want to race ?" circuits
    circuit <- getCircuit (directory </> selectedCircuit)
    _ <- system "clear"
    case circuit of
        Just c -> playCircuit c
        Nothing -> putStrLn "Error reading the circuit"
