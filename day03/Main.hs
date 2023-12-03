module Main where

import qualified Common
import Data.Char (isDigit)
import qualified Data.Map as M

import Debug.Trace

main :: IO ()
main = do
    putStrLn $ "-- Solving day03 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = sum . concat $ M.elems parsedInput
    let answer2 = sum . map (product . snd) . M.toList
                $ M.filterWithKey (\(c,_) ns -> c == '*' && length ns == 2)
                                  parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Vec2 = (Int,Int)
type Part = (Char,Vec2)
type PartNumbersMap = M.Map Part [Int]

parse :: String -> PartNumbersMap
parse input = go M.empty (0,0)
    where asLines = lines input
          (numRows,numCols) = (length asLines, length $ head asLines)
          lineRemainder (x,y) = drop x $ asLines !! y
          get = head . lineRemainder
          go m (x,y)
            | y >= numRows        = m
            | x >= numCols        = go m (0,y+1)
            | isDigit $ get (x,y) =
                let numStr = takeWhile isDigit
                           $ lineRemainder (x,y)
                    numLen = length numStr
                    num = read numStr
                    adjacent = [(c,(x',y')) | x' <- [x-1..x+numLen],
                                              y' <- [y-1..y+1],
                                              x' >=0 && y' >= 0,
                                              x' < numCols && y' < numRows,
                                              let c = get (x', y'),
                                              c /= '.' && (not $ isDigit c)]
                    m' = M.unionWith (++) m
                        . M.fromList . zip adjacent $ repeat [num]
                 in go m' (x+numLen,y)
            | otherwise = go m (x+1,y)
