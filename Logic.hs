module Logic where

import Control.Monad
import Prelude
import Control.Monad.State
import Data.Maybe (isJust)
import Data.List (drop, foldr, map, nub, take, transpose)
import Data.Map ()
import Debug.Trace ()
import System.IO ()
import System.Random (Random (randomRs), RandomGen, getStdGen)
import System.Console.ANSI

import Helpers

------------------- Definitions (Game State) -------------------

data Status a = Mine | Unexplored | Clue a deriving(Show, Eq)

type Location = (Int, Int)

type Board = [[Status Int]]

type ClueMatrix = [[Int]]

type Explored = [[Status Int]]

data Player = Player1 | Player2 deriving(Show, Eq)

data ClColor = ClBlack 
  | ClRed 
  | ClGreen 
  | ClYellow 
  | ClBlue 
  | ClMagenta 
  | ClCyan 
  | ClWhite 
  | ClPurple 
  | ClGrey 
  | ClDarkRed

data GameState = GS {
  player :: Player,
  score1 :: Int,
  score2 :: Int
} deriving(Show, Eq)

size = 2 -- the size of each cell

width = 20 -- the width of the board

height = 20 -- the height of the board

------------------------------- Scoring and GameState -----------------------------

initialState :: GameState
initialState = GS { player = Player1, score1 = 0, score2 = 0 }

-- Function to update score1
updateScore1 :: State GameState ()
updateScore1 = modify (\state -> state { score1 = score1 state + 1 })

-- Function to update score2
updateScore2 :: State GameState ()
updateScore2 = modify (\state -> state { score2 = score2 state + 1 })

-- Function to get the current score1
getScore1 :: State GameState Int
getScore1 = gets score1

-- Function to get the current score2
getScore2 :: State GameState Int
getScore2 = gets score2

updatePlayer :: State GameState ()
updatePlayer = modify (\state -> state { player = switch (player state) })

-- Function to update player
switch :: Player -> Player
switch Player1 = Player2
switch Player2 = Player1



--------------------------------- Board Exploration ------------------------------

{-This function handles the situation where the player wants to explore a
location.-}
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
countVisibleMine :: Explored -> Int
countVisibleMine x = case x of
  [] -> 0
  x : xs -> helper x + countVisibleMine xs
  where
    helper = foldr g 0
    g Mine acc = acc + 1
    g _ acc = acc

{-This function returns true if the winning condition is met-}
winingCondition :: Explored -> GameState -> Bool
winingCondition e state =
  let s1 = score1 state in
    let s2 = score2 state in (s1 > (width * height `div` 10) `div` 2) || (s2 > (width * height `div` 10) `div` 2)

------------------------ Generation of the Game -------------------------------

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
    combine :: Status a -> a -> Status a
    combine Mine _ = Mine
    combine Unexplored _ = Unexplored
    combine (Clue _) x = Clue x

-------------------------- Printing out the Board ----------------------------


coloredText :: ClColor -> String -> String
coloredText c text = "\x1b[38;5;" ++ show (colorCode c) ++ "m" ++ text ++ "\x1b[0m"

colorCode :: ClColor -> Int
colorCode ClBlack = 0
colorCode ClRed = 1
colorCode ClGreen = 2
colorCode ClYellow = 3
colorCode ClBlue = 4
colorCode ClMagenta = 5
colorCode ClCyan = 6
colorCode ClWhite = 7
colorCode ClPurple = 128  -- ANSI color code for purple
colorCode ClGrey = 242    -- ANSI color code for grey
colorCode ClDarkRed = 52  -- ANSI color code for dark red

tile :: Status Int -> String
tile Mine = showCentered size (coloredText ClRed " * ")
tile Unexplored = showCentered size " # "
tile (Clue 0) = showCentered size "   "
tile (Clue 1) = showCentered size (coloredText ClBlue " 1 ")
tile (Clue 2) = showCentered size (coloredText ClGreen " 2 ")
tile (Clue 3) = showCentered size (coloredText ClYellow " 3 ")
tile (Clue 4) = showCentered size (coloredText ClPurple " 4 ")
tile (Clue 5) = showCentered size (coloredText ClDarkRed " 5 ")
tile (Clue 6) = showCentered size (coloredText ClCyan " 6 ")
tile (Clue 7) = showCentered size (coloredText ClBlack " 7 ")
tile (Clue 8) = showCentered size (coloredText ClGrey " 8 ")

showCentered :: Int -> String -> String
showCentered w x = replicate leftPad ' ' ++ x ++ replicate rightPad ' '
  where
    leftPad = w `div` 2
    rightPad = w - leftPad - length x

showMatrixWith :: (a -> String) -> [[a]] -> String
showMatrixWith f = unlines . addBorder . Data.List.map concat . matrixMap f . transpose

{-Adds a border around a list of strings-}
addBorder :: [String] -> [String]
addBorder xs =
  ["   |" ++ horizontalCoordinate (width - 1) 0 ++ "|"]
    ++ ["   " ++ horizontal w]
    ++ zipWith vertical [0 ..] xs
    ++ ["   " ++ horizontal w]
    ++ ["   |" ++ horizontalCoordinate (width - 1) 0 ++ "|"]
  where
    w = length (head xs)
    h = length xs
    horizontal w = "+" ++ replicate (4*width) '-' ++ "+"
    vertical i xs = if i < 10 then show i ++ "  |" ++ xs ++ "| " ++ show i else show i ++ " |" ++ xs ++ "| " ++ show i
    horizontalCoordinate width n
        | n == width  = if n < 10 then showCentered size (" " ++ show n ++ " ") else showCentered size (show n ++ " ")
        | n < 10 = showCentered size (" " ++ show n ++ " ") ++ horizontalCoordinate width (n + 1)
        | otherwise = showCentered size (show n ++ " ") ++ horizontalCoordinate width (n + 1)