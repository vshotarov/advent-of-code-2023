module Main where

import qualified Common
import Data.List.Split (splitOn)
import Data.List (elemIndex)
import Data.Char (ord)
import qualified Data.Map as M

main :: IO ()
main = do
    putStrLn $ "-- Solving day15 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- process
    let (instructions,strings) = unzip parsedInput
    let answer1 = sum $ map hash strings
    let process boxes [] = boxes
        process boxes ((Add l f b):is) =
            let (labels,fLengths) = M.findWithDefault ([],[]) b boxes
              in case l `elemIndex` labels of
                   Just idx -> let (before,after) = splitAt idx fLengths
                                   fLengths' = before ++ [f] ++ tail after
                                   boxes' = M.insert b (labels,fLengths') boxes
                                in process boxes' is
                   Nothing  -> process (M.insert b (labels++[l],fLengths++[f]) boxes) is
        process boxes ((Remove l b):is) =
            let (labels,fLengths) = M.findWithDefault ([],[]) b boxes
              in case l `elemIndex` labels of
                   Just idx -> let labels' = take idx labels ++ drop (idx+1) labels
                                   fLengths' = take idx fLengths ++ drop (idx+1) fLengths
                                   boxes' = M.insert b (labels',fLengths') boxes
                                in process boxes' is
                   Nothing  -> process boxes is
    let answer2 = sum . concatMap (\(b,(_,fls)) -> zipWith (focus b) [1..] fls)
                . M.toList $ process M.empty instructions
            where focus b p fl = (b+1) * p * fl

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

data Instruction = Add String Int Int
                 | Remove String Int
                  deriving (Show)

hash :: String -> Int
hash = go 0
    where go v [] = v
          go v (x:xs) = go ((17 * (v+ord x)) `mod` 256) xs

parse :: String -> [(Instruction, String)]
parse input = map (\x -> (toInstruction x, x)) $ splitOn "," input
    where toInstruction x
            | '=' `elem` x = let (label,focalLength) = Common.splitOnceOn "=" x
                              in Add label (read focalLength) (hash label)
            | otherwise    = Remove (init x) (hash $ init x)
