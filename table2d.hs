
module Table2D where

import Data.Maybe (listToMaybe)
import Data.List (elemIndices)

data Table2D a = [[a]]
type Position = (Int, Int)

empty :: Table2D a -> Bool
empty [] = True
empty [_] = False

telemIndex :: (Eq a) => Table2D -> a -> Maybe Position
telemIndex = (listToMaybe .) . telemIndices

telemIndices :: (Eq a) => Table2D -> a -> [Position]
telemIndices c t = [ (x,y) | (y, line) <- zip [0..] c, x <- elemIndices t line ]

