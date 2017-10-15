module Game2048 where

import Data.List
import System.IO.Unsafe
import System.Random
import Data.List.Split

newtype GameRow = GameRow [Int]
  deriving Eq

data GameBoard = GameBoard { rows :: [GameRow], penalty :: Int }
  deriving Eq

data Move = MoveRight | MoveLeft | MoveUp | MoveDown
  deriving (Eq, Show)

emptyRow :: GameRow
emptyRow = GameRow [0, 0, 0, 0]

emptyGameBoard :: GameBoard
emptyGameBoard = GameBoard [emptyRow, emptyRow, emptyRow, emptyRow] 0

initialBoard :: IO GameBoard
initialBoard = addRandomNumber emptyGameBoard

fromMatrix :: [[Int]] -> GameBoard
fromMatrix xss = GameBoard (map GameRow xss) 0

toMatrix :: GameBoard -> [[Int]]
toMatrix (GameBoard rows _) = map unwrap rows

unwrap :: GameRow -> [Int]
unwrap (GameRow xs) = xs

instance Show GameRow where
  show (GameRow row) = concat ["|", intercalate "|" $ fmap (toSpacedString 5) row, "|"] where
    toSpacedString spaces number = freeSpaces ++ strNum ++ " " where
      strNum = show number
      freeSpaces = replicate (spaces - length strNum) ' '

instance Show GameBoard where
  show (GameBoard rows penalty) =
    replicate 29 '-' ++ "\n" ++ intercalate "\n" (fmap show rows) ++ "\n" ++ replicate 29 '-' ++ "\nPenalty: " ++ show penalty ++ "\n" ++ replicate 29 '-'

doAddition :: ([Int], Int) -> Int -> ([Int], Int)
doAddition (xs, last) current | last == 0 = (xs, current)
                              | last == current = (xs ++ [current + last], 0)
                              | otherwise       = (xs ++ [last], current)

addRow :: [Int] -> [Int]
addRow xs = do
  let (ys, y) = foldl doAddition ([], 0) xs
  if y == 0 then ys
  else ys ++ [y]

nonZeros :: [Int] -> [Int]
nonZeros = filter (/= 0)

zeros :: [Int] -> [Int]
zeros xs = replicate (4 - length xs) 0

moveLeft :: [Int] -> ([Int], Int)
moveLeft xs = (compactHorizontal ++ zeros compactHorizontal, 0) where
  compactHorizontal = addRow $ nonZeros xs

moveRight :: [Int] -> ([Int], Int)
moveRight xs = (zeros compactHorizontal ++ compactHorizontal, 0) where
  compactHorizontal = reverse $ addRow $ reverse $ nonZeros xs


shiftBoard :: GameBoard -> Move -> GameBoard
shiftBoard (GameBoard rows penalty) MoveLeft  = fromMatrix $ map (fst . moveLeft . unwrap) rows
shiftBoard (GameBoard rows penalty) MoveRight = fromMatrix $ map (fst . moveRight . unwrap) rows
shiftBoard (GameBoard rows penalty) MoveDown  = fromMatrix $ transpose $ map (fst . moveRight) $ transpose $ map unwrap rows
shiftBoard (GameBoard rows penalty) MoveUp    = fromMatrix $ transpose $ map (fst . moveLeft) $ transpose $ map unwrap rows

twoOrFour :: IO Int
twoOrFour = do
  rnd <- randomRIO (1,10)  :: IO Int
  if rnd == 10 then return 4
               else return 2

zeroPositions :: GameBoard -> [Int]
zeroPositions g = fst $ foldl accumulatePositionsOfZero ([], 0) (concat $ toMatrix g) where
  accumulatePositionsOfZero (xs, i)  0 = (xs ++ [i], i+1)
  accumulatePositionsOfZero (xs, i)  _ = (xs, i+1)

placeAt :: GameBoard -> Int -> Int -> GameBoard
placeAt g position n = fromMatrix $ chunksOf 4 replacedFlatboard  where
  flatboard = (concat . toMatrix) g
  replacedFlatboard = take position flatboard ++ [n] ++ drop (position + 1) flatboard

randomlyPlace :: Int -> GameBoard -> IO GameBoard
randomlyPlace n g = do
  let z = zeroPositions g
  rnd <- randomRIO (0, length z - 1) :: IO Int
  return $ placeAt g (z !! rnd) n

addRandomNumber :: GameBoard -> IO GameBoard
addRandomNumber g = do
  n <- twoOrFour
  randomlyPlace n g

step :: GameBoard -> Move -> IO GameBoard
step board move = finalBoard where
  shiftedBoard = shiftBoard board move
  finalBoard = if shiftedBoard == board then pure board
                                        else addRandomNumber shiftedBoard

score :: GameBoard -> Int
score = sum . concat . toMatrix
