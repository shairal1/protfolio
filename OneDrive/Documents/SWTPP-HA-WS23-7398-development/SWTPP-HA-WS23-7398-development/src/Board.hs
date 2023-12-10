
module Board where  -- do NOT CHANGE export of module

import Data.Char
import Data.List.Split




-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO ANY CHANGES TO package.yaml, e.g.:
--       import Data.Chars


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

validPosition :: String -> Bool
validPosition = all (`elem` "rb")
validRow :: String -> Bool
validRow x =
  let rowElems = splitOn "," x
  in length rowElems == 6 && all validPosition rowElems
validateFEN :: String -> Bool
validateFEN x =
  let oneRow = splitOn "/" x
  in length oneRow == 6 && all validRow oneRow


-- let fenString = "rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb"
--  putStrLn $ "Is FEN String valid? " ++ show (validateFEN fenString)


-- #############################################################################
-- ####################### buildBoard :: String -> Board #######################
-- ####################### - 2 Functional Points         #######################
-- ####################### - 1 Coverage Point            #######################
-- #############################################################################
buildBoard :: [Char] -> [[Cell]]
buildBoard s = buildBoardApply (splitOn "/" s) [[]]
buildBoardApply :: [[Char]] -> [[Cell]] -> [[Cell]]
buildBoardApply [] s = s
buildBoardApply (r:t) [[]] = buildBoardApply t [buildRow (splitOn "," r) []] --Builds the first row and sets it as start.
buildBoardApply (r:t) c = let thisRow = buildRow (splitOn "," r) [] --Builds the row
  in buildBoardApply t (c ++ [thisRow]) -- Adds the row to the other Cells
buildRow :: [[Char]] -> [Cell] -> [Cell]
buildRow [] c = c
buildRow (h:t) [] = buildRow t [ reverseOriginal h]
buildRow (h:t) c = buildRow t (c ++ [ reverseOriginal h])
reverseOriginal :: [Char] -> Cell
reverseOriginal x= stacking ( reverse x )
--- brr
--for stacking 
stacking :: String -> Cell
stacking ""     = Empty
stacking (x:xs) = Stack (map assignPlayer (x:xs))

assignPlayer :: Char -> Player
assignPlayer 'r' = Red
assignPlayer 'b' = Blue
-- | Helper function to initialize a cell from a string##################################################
-- #################### path :: Pos -> Dir -> Int -> [Pos]  ####################
-- #################### - 4 Functional Points               ####################
-- #################### - 1 Coverage Point                  ####################
-- #############################################################################

reverseDirection :: Dir -> Dir
reverseDirection direction = case direction of
  North      -> South
  NorthEast  -> SouthWest
  East       -> West
  SouthEast  -> NorthWest
  South      -> North
  SouthWest  -> NorthEast
  West       -> East
  NorthWest  -> SouthEast



move :: Pos -> Dir -> Pos
move currentPos direction = case direction of
                  North      -> Pos (col currentPos) (row currentPos + 1)
                  NorthEast  -> Pos (succ (col currentPos)) (row currentPos + 1)
                  East       -> Pos (succ (col currentPos)) (row currentPos)
                  SouthEast  -> Pos (succ (col currentPos)) (row currentPos - 1)
                  South      -> Pos (col currentPos) (row currentPos - 1)
                  SouthWest  -> Pos (pred (col currentPos)) (row currentPos - 1)
                  West       -> Pos (pred (col currentPos)) (row currentPos)
                  NorthWest  -> Pos (pred (col currentPos)) (row currentPos + 1)


path :: Pos -> Dir -> Int -> [Pos]
path start initialDir totalSteps =
  let (positions, stepCount) = unzip $ take (totalSteps + 1) $ iterate (\(pos, count) ->
        let direction = if count < 5 then initialDir else reverseDirection initialDir
        in (move pos direction, count + 1)) (start, 0)
  in positions


{-tba can it be assumed that a valid direction is given 
  path Pos {col = 'a', row = 1}   West 9
[Pos {col = 'a', row = 1},Pos {col = '`', row = 1},Pos {col = '_', row = 1},Pos {col = '^', row = 1},Pos {col = ']', row = 1},Pos {col = '\\', row = 1},Pos {col = ']', row = 1},Pos {col = '^', row = 1},Pos {col = '_', row = 1},Pos {col = '`', row = 1}]
ghci> path Pos {col = 'a', row = 1}   West 9-}

{- should be checked if valid Board
playerWon  [[Empty,Empty,Empty,Empty,Empty,Empty],
[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],
[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],
[Stack [Blue,Blue],Stack [Blue,Blue,Red],Stack [Blue,Blue],Stack [Blue,Blue],--here red is low ,then 2 blue ???
Stack [Blue,Blue],Stack [Blue,Blue]]]
Just Blue-}
{-what is top level defination-}


--y########################################################################################
----Dont forget to shift in other file 
{-playerWon :: Board -> Maybe Player
playerWon board
  | checkWin Blue board = Just Blue
  | checkWin Red board  = Just Red
  | otherwise           = Nothing

checkWin :: Player -> Board -> Bool
checkWin player = all (all isPlayer)
  where
    isPlayer (Stack (top:_)) = player == top
    isPlayer Empty           = True
    isPlayer _               = False-}