module Main where

import           Data.List.Split
import           Day8
import           Debug.Trace
import           System.Environment             ( getArgs )
import           System.IO
import           Text.Printf

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
