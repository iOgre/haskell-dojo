module TTT.A1 where
import Data.Char (toUpper)


-- Q#01
_SIZE_ :: Integer
_SIZE_ = 3

-- Q#02
_DISPLAY_LOGO_ :: Bool
_DISPLAY_LOGO_ = True

-- Q#03
convertRowIndex :: Char -> Int
convertRowIndex c = fromEnum(toUpper c) - 65

-- Q#04
_INVALID_MOVE_ :: (Int, Int)
_INVALID_MOVE_ = (-1, -1)

-- Q#05
_SEP_ :: [Char]
_SEP_ =  ['_','|','_']

-- Q#06

data Square = X | O | Empty deriving (Show, Eq)

-- Q#07

data GameState = XWON | OWON | TIE | INPROGRESS deriving (Show, Eq)

-- Q#08
type Player = Square
type Row = [Square ]
type Line = [Square]
type Board = [Row]
type Move = (Int, Int)
-- Q#09

getFirstPlayer :: Bool -> Player
getFirstPlayer pl = if pl then X else O

getFirstPlayer_ :: Bool -> Player
getFirstPlayer_ pl
    | pl = X
    | not pl = O
    | otherwise = Empty


-- Q#10
showGameState :: GameState -> String
showGameState  state = case state of 
  XWON -> "Player X has won"
  OWON -> "Player O has won"
  TIE -> "It is a TIE"
  INPROGRESS -> "Game is ON"

-- Q#11
switchPlayer :: Player->Player
switchPlayer X = O
switchPlayer O = X
switchPlayer Empty = Empty



-- Q#12
showSquare :: Square -> String
showSquare s = case s of  
  X -> show X
  O -> show O
  Empty -> "_"
