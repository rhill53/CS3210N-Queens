{-# LANGUAGE ParallelListComp #-}

-- CS 3210 - Principles of Programming Languages - Spring 2020
-- Programming Assignment 02 - The N-queens Problem
-- Author(s):

import Data.List

type Seq   = [Char]
type Board = [Seq]

-- TODO 01/17
setup :: Int -> Board
setup n = setup' n n
setup' n 0 = [[]]
setup' n k = [x:y
  | n < 4 , y <- setup' 4 (k-1), x <- [1..4]
  | n > 4 , y <- setup' n (k-1), x <- [1..n]

-- TODO 02/17
rows :: Board -> Int
rows b = length b

-- TODO 03/17
cols :: Board -> Int
cols b
  | length colsPerRow
  where
    colsPerRow = [ length row | row <- b ]

-- TODO 04/17
size :: Board -> (Int, Int)
size x, y
  | x <- cols, y <- rows

-- TODO 05/17
queensSeq :: Seq -> Int
queensSeq s
  | length count
  where
    count = findIndices "Q" s 

-- TODO 06/17
queensBoard :: Board -> Int
queensBoard b
  

-- TODO 07/17
seqValid :: Seq -> Bool
seqValid s = False

-- TODO 08/17
rowsValid :: Board -> Bool
rowsValid b = False

-- TODO 09/17
colsValid :: Board -> Bool
colsValid b = False

-- TODO 10/17
diagonals :: Board -> Int
diagonals b = 0

mainDiagIndices :: Board -> Int -> [ (Int, Int) ]
mainDiagIndices b p
  | p < n = [ (n - 1 - qr, q) | q <- [0..p] | qr <- [p,p-1..0] ]
  | otherwise = [ (q, (n - 1 - qr)) | q <- [0..2 * (n - 1) - p] | qr <- [2 * (n - 1) - p,2 * (n - 1) - p - 1..0] ]
  where n = size b

-- TODO 11/17
allMainDiagIndices :: Board -> [[ (Int, Int) ]]
allMainDiagIndices b = [[]]

-- TODO 12/17
mainDiag :: Board -> [Seq]
mainDiag b = []

secDiagIndices :: Board -> Int -> [ (Int, Int) ]
secDiagIndices b p
  | p < n = [ (p - q, q) | q <- [0..p] ]
  | otherwise = [ (p - (n - 1 - q), n - 1 - q) | q <- [2 * (n - 1) - p, 2 * (n - 1) - p - 1..0] ]
  where n = size b

-- TODO 13/17
allSecDiagIndices :: Board -> [[ (Int, Int) ]]
allSecDiagIndices b = [[]]

-- TODO 14/17
secDiag :: Board -> [Seq]
secDiag b = []

-- TODO 15/17
diagsValid :: Board -> Bool
diagsValid b = False

-- TODO 16/17
valid :: Board -> Bool
valid b = False

-- TODO 17/17 (¡Phew!)
solved :: Board -> Bool
solved b = False

setQueenAt :: Board -> Int -> [Board]
setQueenAt b i = do
  let z = replicate ((size b) - 1) '-'
  let p = nub (permutations ("Q" ++ z))
  [ [ (b!!k) | k <- [0..(i-1)] ] ++ [r] ++ [ (b!!k) | k <- [(i+1)..((rows b) - 1)] ] | r <- p ]

nextRow :: Board -> Int
nextRow b = head [ i | i <- [0 .. (size b) - 1], queensSeq (b!!i) == 0 ]

solve :: Board -> [Board]
solve b
  | solved b = [b]
  | otherwise = concat [ solve newB | newB <- setQueenAt b i, valid newB ]
    where i = nextRow b

main = do
  let b = setup 6
  let solution = [ solution | solution <- solve b ]
  print (solution)
