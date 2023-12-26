module Main where

import qualified Common
import qualified Data.Set as S
import System.Random (randomR, mkStdGen, RandomGen)

main :: IO ()
main = do
    putStrLn $ "-- Solving day25 --"
    input <- Common.readInput

    -- Parse
    let parsedInput@(verts,edges) = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let hasThreeEdges (a:_) = 3 == (length $ filter getEdge edges)
            where getEdge (v1,v2) = 1 == (S.size . S.intersection a $ S.fromList [v1,v2])
        hasThreeEdges _ = error "unreachable"
    let answer1 = product . map S.size . snd
                . Common.firstWhere (hasThreeEdges . snd) . tail
                $ iterate (\(rng',_) -> mincut rng' verts edges) (mkStdGen 25,[])
    let answer2 = "got em stars"

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Vertex = String
type Edge = (Vertex, Vertex)

parse :: String -> (S.Set (S.Set Vertex), [Edge])
parse = foldr parseOne (S.empty, []) . lines
    where parseOne line (vs,es) = let (a,b) = Common.splitOnceOn ": " line
                                      bs = words b
                                      subsets = map S.singleton $ [a] ++ bs
                                      edges = map (\b' -> (a,b')) bs
                                   in (S.union vs $ S.fromList subsets,
                                       es ++ edges)

mincut :: RandomGen g => g -> S.Set (S.Set Vertex) -> [Edge] -> (g,[S.Set Vertex])
mincut rng subsets edges
  | S.size subsets == 2 = (rng, S.toList subsets)
    | otherwise = let randomEdge stdGen = randomR (0, length edges-1) stdGen
                      getSubset v = head . S.toList . S.filter (v `S.member`)
                      (eid,rng') = randomEdge rng
                      (v1,v2) = edges !! eid
                      (s1,s2) = (getSubset v1 subsets, getSubset v2 subsets)
                      subsets' = S.insert (S.union s1 s2)
                               . S.delete s1 $ S.delete s2 subsets
                   in if s1 == s2
                         then mincut rng' subsets edges
                         else mincut rng' subsets' edges
