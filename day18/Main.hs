module Main where

import qualified Common
import Data.Char (isDigit)

main :: IO ()
main = do
    putStrLn $ "-- Solving day18 --"
    input <- Common.readInput

    -- Parse
    let parsedInput@(input1,input2) = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let solve instructions = let edgePoints = dig (0,0) instructions
                                 edges = zip edgePoints $ tail edgePoints
                                 expand (((x1,y1),(x2,y2)),(nx,ny)) =
                                    [(x1+nx/2,y1+ny/2),(x2+nx/2,y2+ny/2)]
                              in area . concatMap expand
                               $ zip edges (normals True edges)
    let answer1 = solve input1
    let answer2 = solve input2

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2


parse :: String -> ([(Char, Int)],[(Char, Int)])
parse input = unzip . map parseOne $ lines input
    where parseOne line = let asWords = words line
                              (d,l,c) = (asWords !! 0, asWords !! 1, asWords !! 2)
                              cc = drop 2 $ init c
                              ll = hexToInt $ take 5 cc
                              dd = case last cc of
                                     '0' -> 'R'
                                     '1' -> 'D'
                                     '2' -> 'L'
                                     '3' -> 'U'
                                     _   -> error "malformatted direction instruction"
                           in ((head d, read l),(dd,ll))

hexToInt :: String -> Int
hexToInt str = go (length str - 1) str
    where go _ [] = 0
          go n (x:xs)
            | isDigit x = (read [x])*(16^n) + go (n-1) xs
            | x == 'a'  = (10*16^n) + go (n-1) xs
            | x == 'b'  = (11*16^n) + go (n-1) xs
            | x == 'c'  = (12*16^n) + go (n-1) xs
            | x == 'd'  = (13*16^n) + go (n-1) xs
            | x == 'e'  = (14*16^n) + go (n-1) xs
            | x == 'f'  = (15*16^n) + go (n-1) xs
            | otherwise = error ("unexpected hexadecimal symbol" ++ [x])

dig :: (Double,Double) -> [(Char, Int)] -> [(Double,Double)]
dig (x,y) [] = [(x,y)]
dig (x,y) ((d,l):instructions) = (x,y):dig (x',y') instructions
    where l' = fromIntegral l
          (x',y') = case d of
                      'U' -> (x,y-l')
                      'R' -> (x+l',y)
                      'D' -> (x,y+l')
                      _   -> (x-l',y)

area :: [(Double,Double)] -> Int
area = go 0
    where go s [] = ceiling (s / 2)
          go s [_] = ceiling (s / 2)
          -- https://en.wikipedia.org/wiki/Shoelace_formula
          go s ((x1,y1):e2@(x2,y2):es) = go (s+(y1+y2)*(x1-x2)) (e2:es)

normals :: Bool -> [((Double,Double),(Double,Double))] -> [(Double,Double)]
normals _ [] = []
normals c (((x1,y1),(x2,y2)):es) =
    let (dx,dy) = (min 1 (max (-1) (x2-x1))
                  ,min 1 (max (-1) (y2-y1)))
        n = if c then (dy,-dx) else (-dy,dx)
     in n:(normals c es)
