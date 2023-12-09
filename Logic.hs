module Logic where

import Control.Monad ()
import Data.List (drop, foldr, map, nub, take, transpose)
import Data.Map ()
import Data.Maybe (isJust)
import Debug.Trace ()
import Helpers (matrixMaker, replaceMatrixIndex, surrounding)
import System.IO ()
import System.Random (Random (randomRs), RandomGen, getStdGen)

-- import Test.HUnit
-- import Test.QuickCheck

------------------- Definitions (Game State) -------------------

data State a = Mine | Unexplored | Clue a deriving (Show)

type Location = (Int, Int)

type Board = [[State Int]]

type ClueMatrix = [[Int]]

type Explored = [[State Int]]

size = 2 -- the size of each cell

width = 20 -- the width of the board

height = 20 -- the height of the board

{- This function handles the situation where the player wants to explore a
location. -}
explore :: Board -> Location -> Explored -> Explored
explore b l@(x, y) e = case e !! x !! y of
  Unexplored -> updateExplored b l e -- Explore if it was not explored
  _ -> e -- don't do anything otherwise

{-This function will take in the initial board, a location
where we want to explore. and an explored map to return a new
explored map with the updated explored map-}
updateExplored :: Board -> Location -> Explored -> Explored
updateExplored b l@(x, y) e =
  case state of
    -- If the state of the location is Clue 0, the map will explore all adjacent
    -- locations
    Clue 0 -> Data.List.foldr (explore b) newExplored $ surrounding width height l
    _ -> newExplored
  where
    state = b !! x !! y
    newExplored = replaceMatrixIndex l e state

{-This function will take in an explored map and check if mines are visible-}
isVisibleMine :: Explored -> Bool
isVisibleMine x = case x of
  [] -> False
  x : xs -> helper x || isVisibleMine xs
  where
    -- This helper function return True if it is a Mine
    helper x = not $ all aux x
    -- aux return false if it is a Mine
    aux Mine = False
    aux _ = True

{-This function randomly generates n points to have mines within the board-}
genLocs :: (RandomGen g) => Int -> Int -> Int -> g -> [Location]
genLocs w h n g =
  zip (Data.List.take n (randomRs (0, w - 1) g)) (Data.List.drop n $ Data.List.take (n * 2) $ randomRs (0, h - 1) g)

{-This function places generated locations of mines into an empty board-}
genBoard :: Int -> Int -> Int -> [Location] -> Board
genBoard w h n = Data.List.foldr helper (matrixMaker w h (Clue 0))
  where
    -- This function places the Mines into the empty board
    helper p b = replaceMatrixIndex p b Mine

{-This function generates of clueMatrix-}
genClueMatrix :: Int -> Int -> [Location] -> ClueMatrix
genClueMatrix w h mines = Data.List.foldr succPoint clueMatrix surroundingPoints
  where
    surroundingPoints = concatMap (surrounding w h) mines
    clueMatrix = replicate w $ replicate h 0
    succPoint p@(x, y) clueMatrix =
      replaceMatrixIndex p clueMatrix $
        succ (clueMatrix !! x !! y)

genGame :: (RandomGen g) => Int -> Int -> Int -> g -> Board
genGame w h n g = [zipWith combine ms cs | (ms, cs) <- zip mineMap clueMatrix]
  where
    mines = nub $ genLocs w h n g
    clueMatrix = genClueMatrix w h mines
    mineMap = genBoard w h n mines
    combine :: State a -> a -> State a
    combine Mine _ = Mine
    combine Unexplored _ = Unexplored
    combine (Clue _) x = Clue x

-------------------------- Printing out the World ----------------------------

{- Prints out the world -}
showBoard :: Board -> IO ()
showBoard w = putStrLn $ showMatrixWith square w

square :: State Int -> String
square Mine = showCentered size "*"
square Unexplored = showCentered size "#"
square (Clue 0) = showCentered size " "
square (Clue n) = showCentered size (show n)

showCentered :: Int -> String -> String
showCentered w x = replicate leftPad ' ' ++ x ++ replicate rightPad ' '
  where
    leftPad = w `div` 2
    rightPad = w - leftPad - length x

{- Maps a function over a list of lists -}
matrixMap :: (a -> b) -> [[a]] -> [[b]]
matrixMap f = Data.List.map (Data.List.map f)

showMatrixWith :: (a -> String) -> [[a]] -> String
showMatrixWith f = unlines . addBorder . Data.List.map concat . matrixMap f . transpose

{- Adds a border around a list of strings -}
addBorder :: [String] -> [String]
addBorder xs =
  [horizontalBorder w]
    ++ Data.List.map verticalBorder xs
    ++ [horizontalBorder w]
  where
    w = length (head xs)
    h = length xs
    horizontalBorder w = "+" ++ replicate w '-' ++ "+"
    verticalBorder xs = "|" ++ xs ++ "|"

-- | make moves until someone wins
playGame :: Explored -> Board -> IO Explored
playGame e b = do
  showBoard e
  input <- getLine
  let new = explore b (parser input) e
   in if isVisibleMine new then return new else playGame new b
  where
    -- This helper function helps parse the input of the players into a Location
    parser :: String -> Location
    parser str = helper $ Data.List.map read $ words str
      where
        helper (x : y : _) = (x, y)

main :: IO ()
main = do
  g <- getStdGen
  let explored = Helpers.matrixMaker width height Unexplored -- nothing is explored
  let world = genGame width height (width * height `div` 10) g
  playGame explored world >>= showBoard

-----------------------------
-- Test Cases
-----------------------------
