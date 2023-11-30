module TestGameLogic where

import CaptureTheMine.GameLogic
import CaptureTheMine.SinglePlayer
import CaptureTheMine.Client
import Test.HUnit

-- Example game logic tests
testGenLocs :: Test
testGenLocs = TestCase $ do
  let genLocations = genLocs 3 3 2 (mkStdGen 42)
  assertEqual "Generated locations should have correct length" 2 (length genLocations)

-- Example single-player game logic tests
testVisibleMine :: Test
testVisibleMine = TestCase $ do
  let explored = matrixMaker 3 3 Unexplored
      board = genGame 3 3 2 (mkStdGen 42)
      newExplored = explore board (2, 2) explored
  assertBool "Mine should be visible" (isVisibleMine newExplored)

-- Example client tests
testExploreCommand :: Test
testExploreCommand = TestCase $ do
  let explored = matrixMaker 3 3 Unexplored
      board = genGame 3 3 2 (mkStdGen 42)
      newExplored = explore board (1, 1) explored
  assertEqual "Exploring a cell updates the explored map" (Clue 0) (newExplored !! 1 !! 1)

tests :: Test
tests = test
  [ testGenLocs
  , testVisibleMine
  , testExploreCommand
  ]

main :: IO Counts
main = runTestTT tests
