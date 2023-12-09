import Control.Monad.State
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

data Player = Player1 | Player2 deriving(Show, Eq)

data ScoreState = ScoreState
  { player :: Player, 
    score1 :: Int, 
    score2 :: Int
  }
  deriving (Show, Eq)

initialState :: ScoreState
initialState = ScoreState { player = Player1, score1 = 0, score2 = 0 }

-- Function to update score1
updateScore1 :: State ScoreState ()
updateScore1 = modify (\state -> state { score1 = score1 state + 1 })

-- Function to update score2
updateScore2 :: State ScoreState ()
updateScore2 = modify (\state -> state { score2 = score2 state + 1 })

-- Function to get the current score1
getScore1 :: State ScoreState Int
getScore1 = gets score1

-- Function to get the current score2
getScore2 :: State ScoreState Int
getScore2 = gets score2

updatePlayer :: State ScoreState ()
updatePlayer = modify (\state -> state { player = switch (player state) })

-- Function to update player
switch :: Player -> Player
switch Player1 = Player2
switch Player2 = Player1

--------------------------- Score Update System -----------------------------------


-- stateTest :: Test
-- stateTest = 
--   "stateTest" 
--     TestList ~:
--       [
--         switch Player1 ~?= Player2
--       ]