module Main where

import qualified Common
import Data.List.Split (splitOn)
import Data.List (transpose)

main :: IO ()
main = do
    putStrLn $ "-- Solving day13 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let solve isSymmetrical rows = if not (null symmetricalCols)
                                      then head symmetricalCols
                                      else 100 * head symmetricalRows
            where cols = transpose rows
                  symmetricalRows = filter (isSymmetrical rows) [1..(length rows - 1)]
                  symmetricalCols = filter (isSymmetrical cols) [1..(length cols - 1)]

    let isSymmetrical1 seqs i = take (length seqs - i) (reverse (take i seqs))
                             == take i (drop i seqs)

    let answer1 = sum $ map (solve isSymmetrical1) parsedInput

    let isSymmetrical2 seqs i = length (filter (uncurry (/=)) pairs) == 1
            where first = take (length seqs - 1) $ reverse (take i seqs)
                  second = take i $ drop i seqs
                  pairs = zip (concat first) (concat second)

    let answer2 = sum $ map (solve isSymmetrical2) parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> [[String]]
parse input = splitOn [""] $ lines input