module Main where

import           Day8
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit
import           Test.QuickCheck


main :: IO ()
main =
  defaultMainWithOpts [
  testCase "pos" testGetPos,
  testCase "isEdge" testIsEdge,
  testCase "pathToEdge" testPathToEdge,
  testCase "takeUntil" testTakeUntil,
  testCase "getViewingDistance" testGetViewingDistance,
  testProperty "testGetPosProp" propGetPos,
  testProperty "propIsEdge" propIsEdge
  ] mempty

testGetPos :: Assertion
testGetPos = getPos Day8.Up (1, 1) @?= (0, 1)

testIsEdge :: Assertion
testIsEdge = isEdge [[1, 2, 3], [4, 5, 6], [7, 8, 9]] (0, 0) @?= True

testPathToEdge :: Assertion
testPathToEdge = getPathToEdge [[1, 2, 3], [4, 5, 6], [7, 8, 9]] Day8.Down (1, 1) @?= [8]

testTakeUntil :: Assertion
testTakeUntil = takeUntil (<5) [3, 5, 3] @?= [3, 5]

testGetEdgePathsForPoint :: Assertion
testGetEdgePathsForPoint = getEdgePathsForPoint [[1, 2, 3], [4, 5, 6], [7, 8, 9]] (1, 1, 5) @?= [[2], [6], [8], [4]]

testGetViewingDistance :: Assertion
testGetViewingDistance = getViewingDistance 5 [3, 5, 3] @?= 2

propGetPos :: (Integer, Integer) -> Property
propGetPos (x, y) = x > 0 ==> getPos Day8.Up (x, y) == (x - 1, y)

propIsEdge :: Grid -> (Integer, Integer) -> Property
propIsEdge grid (x, y) = x == 0 || y == 0 ==> isEdge grid (x, y)



