module Main where

import qualified Common
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.PQueue.Min as PQ

main :: IO ()
main = do
    putStrLn $ "-- Solving day17 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let (maxX,maxY) = (maximum . map fst $ M.keys parsedInput,
                       maximum . map snd $ M.keys parsedInput)
    let answer1 = walk parsedInput (0,3) (maxX,maxY)
    let answer2 = walk parsedInput (3,10) (maxX,maxY)

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Grid = M.Map (Int,Int) Int

parse :: String -> Grid
parse input = M.fromList . concat
            . zipWith (\y -> zipWith (\x c -> ((x,y),read [c])) [0..]) [0..]
            $ lines input

walk :: Grid -> (Int,Int) -> (Int,Int) -> Int
walk grid (minSteps,maxSteps) goal = go 1000000000 S.empty
                $ PQ.insert (0,0,0,1,0) $ PQ.singleton (0,0,0,0,1)
    where walkNs 0 _ _ _ out = out
          walkNs i nhl (x',y') d@(dx',dy') explore' =
                let n@(x'',y'') = (x'+dx',y'+dy')
                    nhl' = nhl + (grid M.! n)
                    explore'' = if i > maxSteps-minSteps
                                   then explore'
                                   else PQ.insert (nhl',x'',y'',dx',dy') explore'
                 in if M.member n grid
                       then walkNs (i-1) nhl' n d explore''
                       else explore'
          go best _ toExplore
            | PQ.null toExplore = best
          go best seen toExplore =
              let ((hl,x,y,dx,dy),explore) = PQ.deleteFindMin toExplore
               in case () of _
                                | (x,y,dx,dy) `S.member` seen -> go best seen explore
                                | (x,y) == goal -> hl
                                | otherwise -> let seen' = S.insert (x,y,dx,dy) seen
                                                in go best seen'
                                                 . walkNs maxSteps hl (x,y) (-dy,dx)
                                                 $ walkNs maxSteps hl (x,y) (dy,-dx) explore
