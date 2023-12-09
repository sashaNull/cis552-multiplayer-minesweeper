module Helpers where

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

------------- Tests for Helper Functions ---------------

tsurrounding :: Test
tsurrounding =
  "surrounding"
    ~: TestList
      [ surrounding 20 20 (2, 2) ~?= [(1, 3), (2, 3), (3, 3), (1, 2), (3, 2), (1, 1), (2, 1), (3, 1)],
        surrounding 20 20 (0, 0) ~?= [(0, 1), (1, 1), (1, 0)],
        surrounding 50 50 (30, 10) ~?= [(29, 11), (30, 11), (31, 11), (29, 10), (31, 10), (29, 9), (30, 9), (31, 9)]
      ]

tinBounds :: Test
tinBounds =
  "inBounds"
    ~: TestList
      [ inBounds 3 4 (1, 1) ~?= True,
        inBounds 5 5 (7, 2) ~?= False,
        inBounds (-1) 10 (2, 2) ~?= False,
        inBounds 10 10 (0, 10) ~?= False
      ]

tmatrixMaker :: Test
tmatrixMaker =
  "matrixMaker"
    ~: TestList
      [ matrixMaker 2 2 True
          ~?= [[True, True], [True, True]],
        matrixMaker 3 2 1
          ~?= [[1, 1], [1, 1], [1, 1]],
        matrixMaker 1 3 10
          ~?= [[10, 10, 10]]
      ]

treplaceMatrixIndex :: Test
treplaceMatrixIndex =
  "replaceMatrixIndex"
    ~: TestList
      [ replaceMatrixIndex (0, 0) (matrixMaker 2 2 1) 0 ~?= [[0, 1], [1, 1]],
        replaceMatrixIndex (1, 2) (matrixMaker 3 3 1) 0 ~?= [[1, 1, 1], [1, 1, 0], [1, 1, 1]],
        replaceMatrixIndex (1, 3) (matrixMaker 3 3 1) 0 ~?= [[1, 1, 1], [1, 1, 1, 0], [1, 1, 1]]
      ]

runTests :: Test
runTests =
  TestList
    [ tsurrounding,
      tinBounds,
      tmatrixMaker,
      treplaceMatrixIndex
    ]
