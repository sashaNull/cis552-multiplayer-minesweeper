module Single where

import Control.Monad.State
import Data.List (drop, foldr, map, nub, take, transpose)
import Data.Map ()
import Helpers
import Logic
import Parser
import System.Console.ANSI
import System.IO ()
import System.Random (Random (randomRs), RandomGen, getStdGen, newStdGen)
import Prelude

playGame :: Explored -> Board -> GameState -> IO Explored
playGame e b oldstate = do
  clearScreen
  if winingCondition e oldstate b
    then do
      putStrLn "Congratulations!"
      putStrLn $ "You Win! " ++ show (player oldstate) ++ "! "
      putStrLn $ "Player1 Score: " ++ show (score1 oldstate)
      putStrLn $ "Player2 Score: " ++ show (score2 oldstate)
      putStrLn $ "Nummber of Remaining Mines: " ++ show (countVisibleMine b - countVisibleMine e)
      return e
    else
      if countVisibleMine e == countVisibleMine b
        then do
          putStrLn "It's a Draw! Play again! "
          putStrLn $ "Player1 Score: " ++ show (score1 oldstate)
          putStrLn $ "Player2 Score: " ++ show (score2 oldstate)
          putStrLn $ "Nummber of Remaining Mines: " ++ show (countVisibleMine b - countVisibleMine e)
          return e
        else do
          putStrLn $ "It is now " ++ show (player oldstate) ++ "'s turn."
          putStrLn $ "Player1 Score: " ++ show (score1 oldstate)
          putStrLn $ "Player2 Score: " ++ show (score2 oldstate)
          putStrLn $ "Nummber of Remaining Mines: " ++ show (countVisibleMine b - countVisibleMine e)
          showBoard e
          putStrLn "Please input Coordinate to explore: (for example: 0 0)"
          input <- getLine
          let temp = doParse locationParser input
           in case temp of
                Nothing -> playGame e b oldstate
                Just loc ->
                  let new = explore b (fst loc) e
                   in if new == e
                        then do
                          playGame new b oldstate
                        else do
                          let oldcount = countVisibleMine e
                           in let newcount = countVisibleMine new
                               in if newcount > oldcount
                                    then
                                      if player oldstate == Player1
                                        then do
                                          let newstate = execState (do updateScore1) oldstate
                                           in playGame new b newstate
                                        else do
                                          let newstate = execState (do updateScore2) oldstate
                                           in playGame new b newstate
                                    else do
                                      let newstate = execState (do updatePlayer) oldstate
                                       in playGame new b newstate

{-Prints out the world-}
showBoard :: Board -> IO ()
showBoard w = putStrLn $ showMatrixWith tile w

createGame :: IO ()
createGame = do
  g <- newStdGen
  let explored = Helpers.matrixMaker width height Unexplored -- nothing is explored
  let board = genGame width height (width * height `div` 10) g
  playGame explored board initialState >>= showBoard

-----------------------------
-- Test Cases
-----------------------------
