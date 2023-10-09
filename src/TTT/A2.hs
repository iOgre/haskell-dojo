module TTT.A2 where

import Data.List (intercalate)
import TTT.A1

-- Q#01
promptPlayer :: Player -> String
promptPlayer  pl =
    concat ["Player ", show pl, "'s turn: enter a row and column position, (ex.A1)"]

-- Q#02

_RANGE_ = [0 .. _SIZE_ - 1]

-- Q#03
isDigit :: Char -> Bool
isDigit c = c `elem` ['0'..'9']

readDigit :: Char -> Int
readDigit d
    | isDigit d =  read [d]
    | otherwise = -1


-- Q#04

_EMPTY_ROW_ = replicate _SIZE_ Empty

_EMPTY_BOARD_ = replicate _SIZE_ _EMPTY_ROW_

-- Q#05
isTied :: Board -> Bool
isTied b = Empty `notElem` concat b

_TIED_BOARD_ :: Board
_TIED_BOARD_ = [
        [X, O, O]
      , [O, X, X]
      , [O, X, O]
      ]

-- Q#06
indexRowStrings :: [String]->[(Char, String)]
indexRowStrings lstr = zip ['A'..] lstr


-- Q#07
formatLine :: [String] -> String
formatLine lstr = concat [_SEP_, intercalate _SEP_ lstr, _SEP_]

-- Q#08
isMoveInBounds :: Move -> Bool
isMoveInBounds mv = case mv of
    (col, row) -> elem col [1.._SIZE_] && elem row [1.._SIZE_]


-- Q#09
stringToMove :: String -> Move
stringToMove str = case str of
    [col, row] ->
        (convertRowIndex col, readDigit row)
    _ -> _INVALID_MOVE_

-- Q#10

replaceSquareInRow = undefined

