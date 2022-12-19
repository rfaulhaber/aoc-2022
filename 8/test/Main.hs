{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Test.QuickCheck
import           Test.QuickCheck.All

prop_getPos direction (x, y) = getPos direction (x, y) == (x + 1, y)

main = $(quickCheckAll)
