module Main where

import qualified Common
import qualified Data.Map as M
import Data.List (sortBy, sort)

main :: IO ()
main = do
    putStrLn $ "-- Solving day07 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    let cardStrengths1 = M.fromList $ zip (reverse "AKQJT98765432") [0..]
    let cardStrengths2 = M.fromList $ zip (reverse "AKQT98765432J") [0..]

    -- Solve
    let solve score strengths =
            sum . map (\(r,(_,bid)) -> r*bid) . zip [1..]
          $ sortBy (\(a,_) (b,_) -> play score strengths a b) parsedInput
    let answer1 = solve score1 cardStrengths1
    let answer2 = solve score2 cardStrengths2

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Card = Char
type SingleCardScore = M.Map Card Int
type Hand = String

parse :: String -> [(Hand, Int)]
parse input = map parseOne $ lines input
    where parseOne line = let (card,bid) = Common.splitOnceOn " " line
                           in (card, read bid)

score1 :: Hand -> Int
score1 cards =
    let cardCounts = sort . M.elems
                   $ foldr (\x -> M.insertWith (+) x 1) M.empty cards :: [Int]
     in case cardCounts of
          [5]       -> 7
          [1,4]     -> 6
          [2,3]     -> 5
          [1,1,3]   -> 4
          [1,2,2]   -> 3
          [1,1,1,2] -> 2
          _         -> 1

score2 :: Hand -> Int
score2 cards =
    let cm = foldr (\x -> M.insertWith (+) x 1) M.empty cards
        cardCounts = sort $ M.elems cm :: [Int]
        js = M.findWithDefault 0 'J' cm 
     in case cardCounts of
          [5]                   -> 7
          [1,4]     | js /= 0   -> 7
                    | otherwise -> 6
          [2,3]     | js /= 0   -> 7
                    | otherwise -> 5
          [1,1,3]   | js /= 0   -> 6
                    | otherwise -> 4
          [1,2,2]   | js == 2   -> 6
                    | js == 1   -> 5
                    | otherwise -> 3
          [1,1,1,2] | js /= 0   -> 4
                    | otherwise -> 2
          _         | js /=0    -> 2
                    | otherwise -> 1

play :: (Hand -> Int) -> SingleCardScore -> Hand -> Hand -> Ordering
play score strengths a b =
    let (a_score,b_score) = (score a, score b)
     in case compare a_score b_score of
          EQ  -> Common.firstWhere (/=EQ)
               . map (uncurry compare . Common.mapTuple (strengths M.!))
               $ zip a b
          res -> res
