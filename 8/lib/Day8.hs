-- | Day8 lib module

module Day8 where

import           Data.List.Split

type Grid = [[Integer]]
type GridPos = (Integer, Integer, Integer)

data Direction = Up | Down | Left | Right deriving (Eq, Show)

getGrid :: String -> Grid
getGrid = map (map read) . map (tail . splitOn "") . words

getGridDimensions :: Grid -> (Integer, Integer)
getGridDimensions grid =
  (toInteger (length (head grid)), toInteger (length grid))

getNextPos :: Grid -> GridPos -> GridPos
getNextPos grid (x, y, _) =
  let (xmax, ymax) = getGridDimensions grid
      newx         = mod (x + 1) xmax
      newy         = mod (y + 1) ymax
  in  (newx, newy, (grid !! (fromIntegral newx)) !! (fromIntegral newy))

getElement :: Grid -> (Integer, Integer) -> Integer
getElement grid (x, y) = grid !! fromIntegral x !! fromIntegral y

getAdjacent
  :: Grid -> (Integer, Integer) -> (Integer, Integer, Integer, Integer)
getAdjacent grid (x, y) =
  let getElementInGrid = getElement grid
  in  ( getElementInGrid (x - 1, y)
      , getElementInGrid (x, y - 1)
      , getElementInGrid (x + 1, y)
      , getElementInGrid (x, y + 1)
      )

isEdge :: Grid -> (Integer, Integer) -> Bool
isEdge _ (_, 0) = True
isEdge _ (0, _) = True
isEdge grid (x, y) =
  let (xmax, ymax) = getGridDimensions grid
  in  x == (xmax - 1) || y == (ymax - 1)


isOutOfBounds :: Grid -> (Integer, Integer) -> Bool
isOutOfBounds _ (x, y) | x < 0 || y < 0 = True
isOutOfBounds grid (x, y) =
  let (xmax, ymax) = getGridDimensions grid in x >= xmax || y >= ymax

getAdjacentByDirection :: Grid -> Direction -> (Integer, Integer) -> Integer
getAdjacentByDirection grid direction pos =
  getElement grid $ getPos direction pos

getPos :: Direction -> (Integer, Integer) -> (Integer, Integer)
getPos Day8.Up    (x, y) = (x - 1, y)
getPos Day8.Down  (x, y) = (x + 1, y)
getPos Day8.Left  (x, y) = (x, y - 1)
getPos Day8.Right (x, y) = (x, y + 1)

-- a very specific implementation lol
takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p x = let (prefix, suffix) = span p x in prefix ++ take 1 suffix

getPathToEdge :: Grid -> Direction -> (Integer, Integer) -> [Integer]
getPathToEdge grid direction pos =
  map (getElement grid) . takeWhile (not . (isOutOfBounds grid)) $ iterate
    (getPos direction)
    (getPos direction pos)

isVisible :: Grid -> GridPos -> Bool
isVisible grid (x, y, _) | isEdge grid (x, y) = True
isVisible grid (x, y, p) =
  let edgePaths = getEdgePathsForPoint grid (x, y, p)
      visibleByRow =
        map (foldl (&&) True) . map (\row -> map (\n -> p > n) row) $ edgePaths
  in  any id visibleByRow

getEdgePathsForPoint :: Grid -> GridPos -> [[Integer]]
getEdgePathsForPoint grid (x, y, _) =
  let edgePathFns =
        map (getPathToEdge grid) $ [Day8.Up, Day8.Down, Day8.Left, Day8.Right]
  in  map (\fn -> fn (x, y)) edgePathFns

getViewingDistance :: Integer -> [Integer] -> Integer
getViewingDistance _ [] = 0
getViewingDistance p r  = fromIntegral . length . takeUntil (\n -> n < p) $ r

getScore :: Grid -> GridPos -> Integer
getScore grid (x, y, p) =
  let edgePaths = getEdgePathsForPoint grid (x, y, p)
      res = fromIntegral . foldl (*) 1 . map (getViewingDistance p) $ edgePaths
  in  res
