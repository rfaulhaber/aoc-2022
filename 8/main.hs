module Main where

import           Data.List.Split
import           Debug.Trace
import           System.Environment             ( getArgs )
import           System.IO
import           Text.Printf

type Grid = [[Integer]]
type GridPos = (Integer, Integer, Integer)

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

isVisible :: Grid -> GridPos -> Bool
isVisible _ (0, _, _) = True
isVisible _ (_, 0, _) = True
isVisible grid (x, y, _)
  | x
    == fromIntegral (length (head grid))
    -  1
    || y
    == fromIntegral (length grid)
    -  1
  = True
isVisible grid (x, y, p) =
  let (back, top, right, down) = getAdjacent grid (x, y)
  in  (trace ("vals " ++ show (back, top, right, down) ++ " " ++ show p) p)
        >  back
        && p
        >  top
        && p
        >  right
        && p
        >  down

solvePart1 :: String -> Integer
solvePart1 input =
  let grid         = getGrid input
      (xmax, ymax) = getGridDimensions grid
      indices      = [ (x, y) | x <- [0 .. xmax - 1], y <- [0 .. ymax - 1] ]
      elements     = map (\(x, y) -> (x, y, getElement grid (x, y))) indices
  in  fromIntegral . length . filter (== True) . map (isVisible grid) $ elements

solvePart2 :: String -> Integer
solvePart2 = undefined

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
