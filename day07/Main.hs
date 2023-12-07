module Main where

import qualified Common
import qualified Data.Map as M
import Data.List (sortBy)

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
    let cardCounts = foldr (\x -> M.insertWith (+) x 1) M.empty cards
     in case M.elems cardCounts of
          [_] -> 7
          [a,b] -> (max a b) + 2
          ns | length ns == 3 -> if elem 3 ns then 4 else 3
             | length ns == 4 -> 2
             | otherwise -> 1

score2 :: Hand -> Int
score2 cards =
    let cm = foldr (\x -> M.insertWith (+) x 1) M.empty cards
        js = M.findWithDefault 0 'J' cm 
        hasJ = js /= 0
     in case M.elems cm of
          [_] -> 7
          [a,b] | hasJ -> 7
                | otherwise -> (max a b) + 2
          ns@[_,_,_] | elem 3 ns -> if hasJ then 6 else 4
                     | hasJ -> 4 + js
                     | otherwise -> 3
          [_,_,_,_] -> if hasJ then 4 else 2
          _ -> if hasJ then 2 else 1

play :: (Hand -> Int) -> SingleCardScore -> Hand -> Hand -> Ordering
play score cardStrengths a b =
    let (a_score,b_score) = (score a, score b)
     in case compare a_score b_score of
          EQ  -> Common.firstWhere (/=EQ)
               $ map (uncurry compare)
               $ zip (map (cardStrengths M.!) a) (map (cardStrengths M.!) b)
          res -> res
