module Main where

import qualified Common
import qualified Data.Map as M
import Data.List.Split

main :: IO ()
main = do
    putStrLn $ "-- Solving day02 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = map parseOne $ lines input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let getMaxNumCubesInGame (_,subsets) = foldr1 (M.unionWith max) subsets
    let rule1 = M.fromList [("red",12),("green",13),("blue",14)]
    let isGameValid = all (>=0) . M.elems
                    . M.unionWith (-) rule1 . getMaxNumCubesInGame
    let powerOfSet cubes = product $ M.elems cubes

    let answer1 = sum . map fst $ filter isGameValid parsedInput
    let answer2 = sum $ map (powerOfSet . getMaxNumCubesInGame) parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Subset = M.Map String Int
type Game = (Int, [Subset])

parseOne :: String -> Game
parseOne line = (read gameId, map parseOneSubset $ splitOn "; " line')
    where parseOneColour = (\(i,col) -> (col, read i))
                         . Common.splitOnceOn " "
          parseOneSubset = M.fromList . map parseOneColour . splitOn ", "
          (gameId,line') = Common.splitOnceOn ": " $ drop 5 line
