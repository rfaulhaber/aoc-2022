module Main where

import           Data.List
import           Data.List.Split                ( splitOn )
import           System.IO
import           Text.Printf

groupCals :: [String] -> [[String]]
groupCals [] = []
groupCals l =
  let first = takeWhile (\s -> s /= "") l
      next  = drop ((length first) + 1) l
  in  first : groupCals next


getCalsFromFile :: String -> [[Int]]
getCalsFromFile = (map (\arr -> map read arr)) . groupCals . splitOn "\n"

solveFirstPart :: String -> Int
solveFirstPart = maximum . map sum . getCalsFromFile

solveSecondPart :: String -> Int
solveSecondPart = sum . take 3 . reverse . sort . map sum . getCalsFromFile

solve :: FilePath -> IO ()
solve path = do
  handle   <- openFile path ReadMode
  contents <- hGetContents handle
  printf "first part: %d\n"  (solveFirstPart contents)
  printf "second part: %d\n" (solveSecondPart contents)

  hClose handle


main :: IO ()
main = solve "../input.txt"
