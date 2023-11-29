module Main where

import Control.Monad.State
import Data.Map qualified as M
import Data.Maybe (isJust)
import Test.HUnit
import Test.QuickCheck

-----------------------------
-- type definitions (model)
-----------------------------
data Player = X | O deriving (Eq, Show)

data Location = Loc Int Int deriving (Eq, Ord, Show)

type Board = M.Map Location Player

data Game = Game {board :: Board, current :: Player} deriving (Eq, Show)

data End = Win Player | Tie deriving (Eq, Show)

-- | starting board for the game
initialGame :: Game
initialGame = undefined

-- | is the board still playable
checkEnd :: Board -> Maybe End
checkEnd = undefined

-- | is this location a valid move for the player
valid :: Board -> Location -> Bool
valid = undefined

-- | make a move at a particular location
makeMove :: Game -> Location -> Maybe Game
makeMove = undefined

-- | display the current game board
showBoard :: Board -> String
showBoard = undefined

-- | Create a type class for the interface for the main game interface
-- so that it can be tested
class Monad m => Interface m where
  -- ask the current player for their next move
  getMove :: Game -> m Location

  -- send a message to all players
  message :: String -> m ()

  -- send a message to the indicated player
  playerMessage :: Player -> String -> m ()

-- | all valid locations
locations :: [Location]
locations = [Loc x y | x <- [1 .. 3], y <- [1 .. 3]]

-- | make moves until someone wins
playGame :: Interface m => Game -> m ()
playGame game = do
  playerMessage (current game) $ showBoard (board game)
  case checkEnd $ board game of
    Just (Win p) -> message $ "Player " ++ show p ++ " wins!"
    Just Tie -> message "It's a Tie!"
    Nothing -> do
      playerMessage (current game) "It's your turn"
      move <- getMove game
      case makeMove game move of
        Just game' -> playGame game'
        Nothing -> error "BUG: move is invalid!"

instance Interface IO where
  getMove = undefined
  playerMessage = undefined
  message = undefined

main :: IO ()
main = playGame initialGame

-----------------------------
-- Test Cases
-----------------------------

-- Helper function to make writing test cases easier.
-- declared, but not yet implemented
makeBoard :: [[Maybe Player]] -> Board
makeBoard = undefined

-- unit tests for the end game
-- clear from reading this code what is being tested and what it
-- depends on
testCheckEnd :: Test
testCheckEnd =
  TestList
    [ "Win for X" ~: checkEnd winBoard ~?= Just (Win X),
      "Initial is playable" ~: checkEnd (board initialGame) ~?= Nothing,
      "Tie game" ~: checkEnd tieBoard ~?= Just Tie
    ]
  where
    winBoard =
      makeBoard
        [ [Just X, Just X, Just X],
          [Nothing, Just O, Nothing],
          [Nothing, Just O, Nothing]
        ]
    tieBoard =
      makeBoard
        [ [Just X, Just O, Just X],
          [Just O, Just X, Just O],
          [Just O, Just X, Just X]
        ]

-- a quickcheck property about validity
prop_validMove :: Game -> Location -> Bool
prop_validMove game move =
  isJust (makeMove game move) == valid (board game) move

-- Arbitrary instances. These don't need to be complete yet,
-- but you should think about what types you will need to be able
-- to generate random values for.
instance Arbitrary Game where
  arbitrary = Game <$> arbitrary <*> arbitrary

instance Arbitrary Player where
  arbitrary = elements [X, O]

instance Arbitrary Location where
  arbitrary = elements locations

