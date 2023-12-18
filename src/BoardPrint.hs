module BoardPrint where

import Data.List (drop, foldr, map, nub, take, transpose)
import Data.Map ()
import Data.Maybe
import Helpers
import Logic

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
colorCode ClPurple = 128 -- ANSI color code for purple
colorCode ClGrey = 242 -- ANSI color code for grey
colorCode ClDarkRed = 52 -- ANSI color code for dark red

tile :: Status Int -> String
tile Mine = showCentered 2 (coloredText ClRed " * ")
tile Unexplored = showCentered 2 " # "
tile (Clue 0) = showCentered 2 "   "
tile (Clue 1) = showCentered 2 (coloredText ClBlue " 1 ")
tile (Clue 2) = showCentered 2 (coloredText ClGreen " 2 ")
tile (Clue 3) = showCentered 2 (coloredText ClYellow " 3 ")
tile (Clue 4) = showCentered 2 (coloredText ClPurple " 4 ")
tile (Clue 5) = showCentered 2 (coloredText ClDarkRed " 5 ")
tile (Clue 6) = showCentered 2 (coloredText ClCyan " 6 ")
tile (Clue 7) = showCentered 2 (coloredText ClBlack " 7 ")
tile (Clue 8) = showCentered 2 (coloredText ClGrey " 8 ")
tile _ = error "This is impossible"

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
    horizontal w = "+" ++ replicate (4 * width) '-' ++ "+"
    vertical i xs = if i < 10 then show i ++ "  |" ++ xs ++ "| " ++ show i else show i ++ " |" ++ xs ++ "| " ++ show i
    horizontalCoordinate width n
      | n == width = if n < 10 then showCentered 2 (" " ++ show n ++ " ") else showCentered 2 (show n ++ " ")
      | n < 10 = showCentered 2 (" " ++ show n ++ " ") ++ horizontalCoordinate width (n + 1)
      | otherwise = showCentered 2 (show n ++ " ") ++ horizontalCoordinate width (n + 1)
