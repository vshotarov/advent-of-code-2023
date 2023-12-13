module Main where

import qualified Common
import Data.List (transpose)

main :: IO ()
main = do
    putStrLn $ "-- Solving day11 --"
    input <- Common.readInput

    -- Parse
    let input1 = parse 2 input
    let input2 = parse 1000000 input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show (input1,input2))

    -- Solve
    let solve galaxies = (sum [manhattan p1 p2 | p1 <- galaxies,
                                                 p2 <- galaxies,
                                                 p1 /= p2]) `div` 2
            where manhattan (x1,y1) (x2,y2) = abs (x2-x1) + abs (y2-y1)
    let answer1 = solve input1
    let answer2 = solve input2

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Galaxy = (Int,Int)

parse :: Int -> String -> [Galaxy]
parse numExpansions input = map expand galaxies
    where foldLine (y,line) acc = foldr accumGalaxy acc $ zip [0..] line
                where accumGalaxy (x,'#') = ((x,y):)
                      accumGalaxy _ = id
          inputLines = lines input
          galaxies = foldr foldLine [] $ zip [0..] inputLines
          emptyRows = [y | (y,row) <- zip [0..] inputLines,
                           all (=='.') row]
          emptyCols = [x | (x,col) <- zip [0..] $ transpose inputLines,
                           all (=='.') col]
          expand (x,y) = (x' + (numExpansions-1)*length (filter (<x) emptyCols), y')
                where (x',y') = (x,y + (numExpansions-1)*length (filter (<y) emptyRows))