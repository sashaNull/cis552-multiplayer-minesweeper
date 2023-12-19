import Data.List
import Helpers
import Logic
import Parser
import System.Random
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
import Test.QuickCheck

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

runTestsHelper :: Test
runTestsHelper =
  TestList
    [ tsurrounding,
      tinBounds,
      tmatrixMaker,
      treplaceMatrixIndex
    ]

-- Property: Applying matrixMap with the identity function results in the original matrix.
prop_identity :: (Eq a, Show a) => [[a]] -> Property
prop_identity matrix = matrixMap id matrix === matrix

-- Property: Applying matrixMap with two functions is the same as applying them sequentially.
prop_composition :: (Eq b, Eq c, Show c) => Fun a b -> Fun b c -> [[a]] -> Property
prop_composition (Fun _ f) (Fun _ g) matrix = matrixMap (g . f) matrix === matrixMap g (matrixMap f matrix)

propHelpers :: IO ()
propHelpers = do
  putStrLn "Property tests for matrixMap: Identity"
  quickCheck (prop_identity :: [[Int]] -> Property)
  putStrLn "Property tests for matrixMap: Composition"
  quickCheck (prop_composition :: Fun Int Int -> Fun Int Int -> [[Int]] -> Property)

------------------------------- Tests for Logic ------------------------------

instance Arbitrary Player where
  arbitrary :: Gen Player
  arbitrary = oneof [pure Player1, pure Player2]

-- Property: Applying matrixMap with the identity function results in the original matrix.
prop_identity_swtich :: Player -> Property
prop_identity_swtich x = switch (switch x) === x

------------- Tests for explore -----------
instance (Arbitrary a) => Arbitrary (Status a) where
  arbitrary :: (Arbitrary a) => Gen (Status a)
  arbitrary = oneof [pure Mine, pure Unexplored, Clue <$> arbitrary]

prop_explore :: StdGen -> Int -> Int -> Location -> Property
prop_explore g a b l@(x, y) =
  explore
    (genGame (max (min a 100) 1) (max (min b 100) 1) (max (min a 100) 1 * max (min b 100) 1 `div` 10) g)
    (min x (min a 100 - 1), min y (min b 100 - 1))
    (genGame (max (min a 100) 1) (max (min b 100) 1) (max (min a 100) 1 * max (min b 100) 1 `div` 10) g)
    === genGame (max (min a 100) 1) (max (min b 100) 1) (max (min a 100) 1 * max (min b 100) 1 `div` 10) g

texplore :: Test
texplore =
  "explore"
    ~: TestList
      [ explore (matrixMaker 10 10 (Clue 0)) (0, 0) (matrixMaker 10 10 (Clue 0)) ~?= matrixMaker 10 10 (Clue 0),
        explore
          (replaceMatrixIndex (0, 0) (matrixMaker 10 10 (Clue 0)) Mine)
          (0, 0)
          (replaceMatrixIndex (0, 0) (matrixMaker 10 10 (Clue 0)) Unexplored)
          ~?= replaceMatrixIndex (0, 0) (matrixMaker 10 10 (Clue 0)) Mine
      ]

-------------- Tests for countVisibleMine & genGame --------------
instance Arbitrary StdGen where
  arbitrary = mkStdGen <$> arbitrary

testBoard :: StdGen -> Int -> Int -> Board
testBoard g a b = genGame a b (a * b `div` 10) g

prop_count_allmines :: StdGen -> Int -> Int -> Property
prop_count_allmines g a b =
  countVisibleMine (testBoard g (max a 0) (max b 0))
    === length (nub $ genLocs (max a 0) (max b 0) (max a 0 * max b 0 `div` 10) g)

logicTests :: IO ()
logicTests = do
  putStrLn "Property tests for switch"
  quickCheck (prop_identity_swtich :: Player -> Property)
  putStrLn "Property tests for explore and genGame"
  quickCheck (prop_explore :: StdGen -> Int -> Int -> Location -> Property)
  putStrLn "Property tests for countVisibleMine and genGame"
  quickCheck (prop_count_allmines :: StdGen -> Int -> Int -> Property)

main :: IO ()
main = do
  putStrLn "Unit Tests for Helper Functions"
  runTestTT runTestsHelper
  propHelpers
  logicTests
