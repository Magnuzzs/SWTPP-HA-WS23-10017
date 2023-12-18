module Board where  -- do NOT CHANGE export of module

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO ANY CHANGES TO package.yaml, e.g.:
--       import Data.Chars

--import Text.Parsec
--import Text.Parsec.Char


-- #############################################################################
-- ############# GIVEN IMPLEMENTATION                           ################
-- ############# Note: "deriving Show" may be deleted if needed ################
-- #############       Given data types may NOT be changed      ################
-- #############################################################################

data Player = Red | Blue deriving Show
data Cell =  Stack [Player] | Empty deriving Show
data Pos = Pos { col :: Char, row :: Int } deriving Show
data Dir = North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest deriving Show
type Board = [[Cell]]

instance Eq Pos where
  (==) (Pos c1 r1) (Pos c2 r2) = (c1 == c2) && (r1 == r2)

instance Eq Player where
  (==) Blue Blue = True
  (==) Red Red = True
  (==) _ _ = False

instance Eq Cell where
  (==) Empty Empty = True
  (==) (Stack xs) (Stack ys) = xs == ys
  (==) _ _ = False


-- #############################################################################
-- ################# IMPLEMENT validateFEN :: String -> Bool ###################
-- ################## - 2 Functional Points                  ###################
-- ################## - 1 Coverage Point                     ###################
-- #############################################################################

validateFEN :: String -> Bool
validateFEN fenstring = validateHelper1 fenstring && validateHelper2 fenstring

validateHelper1 :: String -> Bool
validateHelper1 fenstring = (filter (\c -> c == ',' || c == '/') fenstring) == ",,,,,/,,,,,/,,,,,/,,,,,/,,,,,/,,,,,"

validateHelper2 :: String -> Bool
validateHelper2 fenstring = (filter (\c -> c == 'r' || c == 'b' || c == ',' || c == '/') fenstring) == fenstring


-- #############################################################################
-- ####################### buildBoard :: String -> Board #######################
-- ####################### - 2 Functional Points         #######################
-- ####################### - 1 Coverage Point            #######################
-- #############################################################################

{-
_playerParser :: Char -> Player
_playerParser 'r' = Red
_playerParser 'b' = Blue

charToPlayer :: Parsec String () Player
charToPlayer = _playerParser <$> oneOf "rb"

_playerToCell :: [Player] -> Cell
_playerToCell [] = Stack[]
_playerToCell cs = Stack cs

cell :: Parsec String () Cell
cell = _playerToCell <$> many charToPlayer

board :: Parsec String () Board
board = sepBy (sepBy cell (char ',')) (char '/')


buildBoard :: String -> Either ParseError Board
buildBoard = parse board ""
-}

-- #############################################################################
-- #################### path :: Pos -> Dir -> Int -> [Pos]  ####################
-- #################### - 4 Functional Points               ####################
-- #################### - 1 Coverage Point                  ####################
-- #############################################################################

moveCol :: Char -> Int -> Char
moveCol c offset = toEnum $ fromEnum c + offset

moveRow :: Int -> Int -> Int
moveRow r offset = r + offset

movePos :: Pos -> Int -> Int -> Pos
movePos (Pos col row) colOffset rowOffset =
  Pos (moveCol col colOffset) (moveRow row rowOffset)

path :: Pos -> Dir -> Int -> [Pos]
path pos _ 0 = [pos]  -- Base case: stop when the integer argument is 0
path pos North int
  | row pos == 6 = path pos South (int)
  | otherwise = pos : path (movePos pos 0 1) North (int-1)
path pos East int
  | col pos == 'f' = path pos West (int)
  | otherwise = pos : path (movePos pos 1 0) East (int-1)
path pos West int
  | col pos == 'a' = path pos East (int)
  | otherwise = pos : path (movePos pos (1) 0) West (int-1)
path pos South int
  | row pos == 1 = path pos North (int)
  | otherwise = pos : path (movePos pos 0 (1)) South (int-1)
path pos SouthEast int
  | col pos == 'f' = path pos SouthWest (int)
  | row pos == 1 = path pos NorthEast (int)
  | otherwise = pos : path (movePos pos (1) (-1)) SouthEast (int-1)
path pos SouthWest int
  | col pos == 'a' = path pos SouthEast (int)
  | row pos == 1 = path pos NorthWest (int)
  | otherwise = pos : path (movePos pos (-1) (-1)) SouthWest (int-1)
path pos NorthEast int
  | col pos == 'f' = path pos NorthWest (int)
  | row pos == 6 = path pos SouthEast (int)
  | otherwise = pos : path (movePos pos 1 (1)) NorthEast (int-1)
path pos NorthWest int
  | col pos == 'a' = path pos NorthEast (int)
  | row pos == 6 = path pos SouthWest (int)
  | otherwise = pos : path (movePos pos (-1) (1)) NorthWest (int-1)