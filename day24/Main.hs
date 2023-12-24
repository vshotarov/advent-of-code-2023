module Main where

import qualified Common
import Data.List.Split (splitOn)

main :: IO ()
main = do
    putStrLn $ "-- Solving day24 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let (low,high) = (200000000000000,400000000000000)
    --let (low,high) = (7,27)
    let answer1 :: Int
        answer1 = sum [1 | (i,a) <- zip [0..] parsedInput :: [(Int,(Vec,Vec))],
                           (j,b) <- zip [0..] parsedInput :: [(Int,(Vec,Vec))],
                           j>i,
                           case intersection (flatOnZ a) (flatOnZ b) of
                             Nothing -> False
                             Just (x,y,_) -> x >= low && x <= high
                                          && y >= low && y <= high]
            where flatOnZ ((px,py,_),(vx,vy,_)) = ((px,py,0),(vx,vy,0))
    let answer2 = 769281292688187 :: Int


    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn "For Part 2 I solved the 9x9 linear system for H1,H2,H3 in sympy."
    putStrLn $ "Part 2: " ++ show answer2

type Vec = (Double,Double,Double)

parse :: String -> [(Vec,Vec)]
parse input = map parseOne $ lines input
    where parseOne line = let (p,v) = Common.splitOnceOn " @ " line
                              toVec [x,y,z] = (x,y,z)
                              toVec _ = error "malformatted input"
                              p' = toVec . map (fromIntegral
                                             . (read :: (String -> Int))) $ splitOn "," p
                              v' = toVec . map (fromIntegral
                                             . (read :: (String -> Int))) $ splitOn "," v
                           in (p',v')

cross :: Vec -> Vec -> Vec
cross (a1,a2,a3) (b1,b2,b3) = (a2*b3 - a3*b2, a3*b1 - a1*b3, a1*b2 - a2*b1)

dot :: Vec -> Vec -> Double
dot (a1,a2,a3) (b1,b2,b3) = a1*b1 + a2*b2 + a3*b3

magnitude :: Vec -> Double
magnitude (x,y,z) = sqrt (x**2 + y**2 + z**2)

normalise :: Vec -> Vec
normalise v@(x,y,z) = (x/mag,y/mag,z/mag)
    where mag = magnitude v

sub :: Vec -> Vec -> Vec
sub (x1,y1,z1) (x2,y2,z2) = (x1-x2,y1-y2,z1-z2)

add :: Vec -> Vec -> Vec
add (x1,y1,z1) (x2,y2,z2) = (x1+x2,y1+y2,z1+z2)

scale :: Double -> Vec -> Vec
scale f (x,y,z) = (x*f,y*f,z*f)

intersection :: (Vec,Vec) -> (Vec,Vec) -> Maybe Vec
intersection ((p1x,p1y,p1z),v1) (p2@(p2x,p2y,p2z),v2) =
    let v1n@(v1x,v1y,v1z) = v1
        v1xv2 = cross v1n v2
        pDiff = (p2x-p1x,p2y-p1y,p2z-p1z)
        pDiffxV2 = cross pDiff v2
        v1xv2Mag = magnitude v1xv2
        a = magnitude pDiffxV2 / v1xv2Mag
        p' = (p1x+a*v1x,p1y+a*v1y,p1z+a*v1z)
        p'p2Diff = sub p' p2
        v2n = normalise v2
        d2 = dot p'p2Diff v2n
        p'2 = add p2 (scale d2 v2n)
        delta = sub p'2 p'
     in if v1xv2Mag < 1 || (magnitude delta) > 1 || d2 < 0
           then Nothing
           else Just p'

