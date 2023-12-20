module Main where

import qualified Common
import Data.List.Split (splitOn)
import qualified Data.Map as M

main :: IO ()
main = do
    putStrLn $ "-- Solving day19 --"
    input <- Common.readInput

    -- Parse
    let parsedInput@(system,parts) = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = sum . map (sum . M.elems)
                $ filter (isAccepted system (system M.! "in")) parts
    let answer2 = solutions system

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Part = M.Map String Int
type Rule = String
type System = M.Map String [Rule]

isAccepted :: System -> [Rule] -> Part -> Bool
isAccepted _ [] _ = error "should never reach empty ruleset"
isAccepted _ ("A":[]) _ = True
isAccepted _ ("R":[]) _ = False
isAccepted system (a:[]) part  = isAccepted system (system M.! a) part
isAccepted system (r:rs) part =
    let eval op (c,v) = op (part M.! c) (read v)
        (cond,action) = Common.splitOnceOn ":" r
        nextRules = M.findWithDefault [action] action system
        next = isAccepted system nextRules part
     in case r !! 1 of
          '>' -> if (eval (>) $ Common.splitOnceOn ">" cond)
                    then next else isAccepted system rs part
          '<' -> if (eval (<) $ Common.splitOnceOn "<" cond)
                    then next else isAccepted system rs part
          _   -> next 

solutions :: System -> Int
solutions system = go [(system M.! "in", M.fromList [(x,(1,4000)) | x <- "xmas"])]
    where go [] = 0
          go (([],_):_) = error "reached empty ruleset"
          go ((["A"],ranges):explore) = (product . map (\(a,b) -> b-a+1) $ M.elems ranges)
                                      + go explore
          go ((["R"],_):explore) = go explore
          go (((rule:rules),ranges):explore) =
              let (cond,action) = Common.splitOnceOn ":" rule
                  nextRules = M.findWithDefault [action] action system
                  c = head cond
                  v = read $ drop 2 cond
                  updateRange (l1,h1) (l2,h2) = (max l1 l2, min h1 h2)
               in case rule !! 1 of

                    '>' -> go ((nextRules,M.insertWith updateRange c (v+1,4000) ranges)
                              :(rules,M.insertWith updateRange c (1,v) ranges)
                              :explore)
                    '<' -> go ((nextRules,M.insertWith updateRange c (1,v-1) ranges)
                              :(rules,M.insertWith updateRange c (v,4000) ranges)
                              :explore)
                    _  -> go ((system M.! cond,ranges):explore)

parse :: String -> (System,[Part])
parse input = let (rules,parts) = Common.splitOnceOn [""] $ lines input
                  rules' = M.map (splitOn ",") . M.fromList
                         $ map (Common.splitOnceOn "{" . init) rules
                  parsePart = M.fromList
                            . map ((\(c,v) -> (c, read v)) . Common.splitOnceOn "=")
                            . splitOn "," . tail . init
                  parts' = map parsePart parts
               in (rules',parts')
