module Main where

import           Data.List
import           System.IO
import           Text.Printf

distinct :: Int -> String -> Bool
distinct size = (== size) . length . nub

windowStr :: Int -> String -> [String]
windowStr _    ""  = []
windowStr size str = [take size str] ++ (windowStr size (drop 1 str))

solvePart1 :: String -> Int
solvePart1 = (+ 4) . inner 0 . windowStr 4
 where
  inner :: Int -> [String] -> Int
  inner n (x : xs) = if distinct 4 x then n else inner (n + 1) xs
  inner _ []       = 0

solveN :: Int -> String -> Int
solveN size = (+ size) . inner 0 . windowStr size
 where
  inner :: Int -> [String] -> Int
  inner n (x : xs) = if distinct size x then n else inner (n + 1) xs
  inner _ []       = 0

solve :: FilePath -> IO ()
solve path = do
  handle   <- openFile path ReadMode
  contents <- hGetContents handle

  printf "part 1 %d\n" (solveN 4 contents)
  printf "part 2 %d\n" (solveN 14 contents)

  hClose handle

main = solve "./input.txt"
