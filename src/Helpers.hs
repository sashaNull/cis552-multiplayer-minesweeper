module Helpers where

import Data.List
import Test.HUnit
  ( Assertion,
    Test (TestList),
    assertFailure,
    runTestTT,
    (@?=),
    (~:),
    (~=?),
    (~?=),
  )
import Test.QuickCheck (Fun (..), Property, quickCheck, (===))

{-This function returns the all the surrounding location of a location-}
surrounding :: Int -> Int -> (Int, Int) -> [(Int, Int)]
surrounding w h (x, y) =
  filter
    (inBounds w h)
    [ (x - 1, y + 1),
      (x, y + 1),
      (x + 1, y + 1),
      (x - 1, y),
      (x + 1, y),
      (x - 1, y - 1),
      (x, y - 1),
      (x + 1, y - 1)
    ]

{-This function check if a location is in the bounds provided-}
inBounds :: Int -> Int -> (Int, Int) -> Bool
inBounds w h (x, y)
  | x < 0 = False
  | x >= w = False
  | y < 0 = False
  | y >= h = False
  | otherwise = True

{-This helper function makes a list of list of a particular element
with the expected size-}
matrixMaker :: Int -> Int -> a -> [[a]]
matrixMaker w h e = replicate w $ replicate h e

{-This helper function replace a Location's element with another element-}
replaceMatrixIndex :: (Int, Int) -> [[a]] -> a -> [[a]]
replaceMatrixIndex (x, y) m e = replaceIndex x m $ replaceIndex y (m !! x) e
  where
    replaceIndex index xs x = take index xs ++ (x : drop (index + 1) xs)

{-Maps a function over a list of lists-}
matrixMap :: (a -> b) -> [[a]] -> [[b]]
matrixMap f = Data.List.map (Data.List.map f)