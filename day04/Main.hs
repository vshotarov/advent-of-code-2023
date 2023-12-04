module Main where

import qualified Common
import Data.List.Split (splitOn)
import qualified Data.Set as S
import qualified Data.Map as M

main :: IO ()
main = do
    putStrLn $ "-- Solving day04 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let getNumMatches = S.size . (uncurry S.intersection)
    let solveOne1 card = case getNumMatches card of
                           0 -> 0
                           n -> 2^(n-1)

    let answer1 = sum $ map solveOne1 parsedInput :: Int

    let solve2 cards = sum $ M.elems counts
            where enumCards = zip [0..] cards :: [(Int,Card)]
                  startCounts = M.fromList $ map (\(i,_) -> (i,1)) enumCards
                  copyOne state (i,card) =
                      let numCopies = state M.! i
                          toCopy = take (getNumMatches card) [(i+1)..]
                          copies = M.fromList . zip toCopy $ repeat numCopies
                       in M.unionWith (+) state copies
                  counts = foldl copyOne startCounts enumCards

    let answer2 = solve2 parsedInput :: Int

    ---- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Card = (S.Set Int, S.Set Int)

parse :: String -> [Card]
parse = map parseOne . lines
    where parseOne line = let lists = map (S.fromList . map read . words)
                                    . splitOn " | "
                                    . snd $ Common.splitOnceOn ": " line
                           in (lists !! 0, lists !! 1)
