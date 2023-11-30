import Control.Monad
import Data.List
import Data.Map qualified as M
import Data.Maybe (isJust)
import Debug.Trace
import System.IO
import System.Random
-- import Test.HUnit
-- import Test.QuickCheck

-----------------------------
-- type definitions (model)
-----------------------------
data State a = Mine | Unexplored | Clue a deriving (Show)

type Location = (Int, Int)

type Board = [[State Int]]

type ClueMatrix = [[Int]]

type Explored = [[State Int]]

size = 2 -- the size of each cell

width = 50 -- the width of the board

height = 50 -- the height of the board

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
    Clue 0 -> foldr (explore b) newExplored $ surrounding width height l
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
    helper x = not $ all aux xs
    -- aux return false if it is a Mine
    aux Mine = False
    aux _ = True

{-This function randomly generates n points to have mines within the board-}
genLocs :: (RandomGen g) => Int -> Int -> Int -> g -> [Location]
genLocs w h n g =
  zip (take n (randomRs (0, w - 1) g)) (drop n $ take (n * 2) $ randomRs (0, h - 1) g)

{-This function places generated locations of mines into an empty board-}
genBoard :: Int -> Int -> Int -> [Location] -> Board
genBoard w h n = foldr helper (matrixMaker w h (Clue 0))
  where
    -- This function places the Mines into the empty board
    helper p b = replaceMatrixIndex p b Mine

{-This function generates of clueMatrix-}
genClueMatrix :: Int -> Int -> [Location] -> ClueMatrix
genClueMatrix w h mines = foldr succPoint clueMatrix surroundingPoints
  where
    surroundingPoints = concatMap (surrounding w h) mines
    clueMatrix = replicate w $ replicate h 0
    succPoint p@(x, y) clueMatrix =
      replaceMatrixIndex p clueMatrix $
        succ (clueMatrix !! x !! y)

{-This function returns the all the surrounding location of a location-}
surrounding :: Int -> Int -> Location -> [Location]
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

-- This function check if a location is in the bounds provided
inBounds :: Int -> Int -> Location -> Bool
inBounds w h (x, y)
  | x < 0 = False
  | x >= w = False
  | y < 0 = False
  | y >= h = False
  | otherwise = True

-- This helper function makes a list of list of a particular element
-- with the expected size
matrixMaker :: Int -> Int -> a -> [[a]]
matrixMaker w h e = replicate w $ replicate h e

-- This helper function replace a Location's element with another element
replaceMatrixIndex :: Location -> [[a]] -> a -> [[a]]
replaceMatrixIndex (x, y) m e = replaceIndex x m $ replaceIndex y (m !! x) e
  where
    replaceIndex index xs x = take index xs ++ (x : drop (index + 1) xs)

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

-- | make moves until someone wins
playGame :: Explored -> Board -> IO Explored
playGame e b = do
  showBoard e
  input <- getLine
  let new = explore b (parser input) e
   in if isVisibleMine new then return new else playGame new w
  where
    -- This helper function helps parse the input of the players into a Location
    parser :: String -> Location
    parser str = helper $ map read $ words str
      where
        helper (x : y : _) = (x, y)

main :: IO ()
main = do
  g <- getStdGen
  explored <- matrixMaker width height Unexplored
  board <- genGame width height (width * height `div` 10) g
  playGame explored board >>= showBoard

-----------------------------
-- Test Cases
-----------------------------
