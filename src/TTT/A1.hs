module TTT.A1 where
import Data.Char (toUpper)
import Data.Sequence (Seq(Empty))


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
type Row = [Square]
type Line = [Square]
type Board = [Row]
type Move = (Int, Int)
-- Q#09

getFirstPlayer = undefined

getFirstPlayer_ = undefined

-- Q#10

showGameState = undefined

-- Q#11

switchPlayer = undefined

-- Q#12

showSquare = undefined