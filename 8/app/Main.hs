module Main where

import           Data.List.Split
import           Debug.Trace
import           System.Environment             ( getArgs )
import           System.IO
import           Text.Printf

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
getPos Main.Up    (x, y) = (x, y - 1)
getPos Main.Down  (x, y) = (x, y + 1)
getPos Main.Left  (x, y) = (x - 1, y)
getPos Main.Right (x, y) = (x + 1, y)

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p = foldr (\x ys -> x : if p x then [] else ys) []

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
        map (getPathToEdge grid) $ [Main.Up, Main.Down, Main.Left, Main.Right]
  in  map (\fn -> fn (x, y)) edgePathFns

getViewingDistance :: Integer -> [Integer] -> Integer
getViewingDistance _ [] = 0
getViewingDistance p r  = fromIntegral . length . takeUntil (\n -> n <= p) $ r

getScore :: Grid -> GridPos -> Integer
getScore grid (x, y, p) =
  let edgePaths = getEdgePathsForPoint grid (x, y, p)
      res = fromIntegral . foldl (*) 1 . map (getViewingDistance p) $ edgePaths
  in  trace ("res: " ++ show (x, y, p) ++ "score " ++ show res) res


solvePart1 :: String -> Integer
solvePart1 input =
  let grid         = getGrid input
      (xmax, ymax) = getGridDimensions grid
      indices      = [ (x, y) | x <- [0 .. xmax - 1], y <- [0 .. ymax - 1] ]
      elements     = map (\(x, y) -> (x, y, getElement grid (x, y))) indices
  in  fromIntegral . length . filter id . map (isVisible grid) $ elements

solvePart2 :: String -> Integer
solvePart2 input =
  let grid         = getGrid input
      (xmax, ymax) = getGridDimensions grid
      indices      = [ (x, y) | x <- [0 .. xmax - 1], y <- [0 .. ymax - 1] ]
      elements     = map (\(x, y) -> (x, y, getElement grid (x, y))) indices
      scores       = map (getScore grid) elements
  in  trace ("scores: " ++ show scores) foldl max 0 scores

solvePart :: Integer -> String -> Integer
solvePart part = if part == 1 then solvePart1 else solvePart2

solve :: IO ()
solve = do
  args     <- getArgs
  handle   <- openFile (args !! 0) ReadMode
  contents <- hGetContents handle

  print (solvePart (read (args !! 1)) contents)

main :: IO ()
main = solve
