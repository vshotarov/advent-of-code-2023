module Main where

import qualified Common
import Data.List (transpose, elemIndex)

main :: IO ()
main = do
    putStrLn $ "-- Solving day14 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = lines input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let slide [] = []
        slide ('.':xs) = let available = takeWhile (/='#') xs
                             numOs = length $ filter (=='O') available
                             numDots = 1 + length available - numOs
                          in replicate numOs 'O' ++ replicate numDots '.'
                          ++ slide (drop (numOs+numDots-1) xs)
        slide (x:xs) = x:slide xs
    let north = transpose . map slide . transpose
    let load rows = sum $ zipWith (\y -> ((nRows-y)*) . countOs) [0..] rows
            where nRows = length rows
                  countOs = length . filter (=='O')
    let answer1 = load $ north parsedInput

    let west = map slide
    let south = transpose . map (reverse . slide . reverse) . transpose
    let east = map (reverse . slide . reverse)
    let answer2 = load (cycles !! billionth)
            where cycles = iterate (east . south . west . north) parsedInput
                  go i prev (c:cs) = case c `elemIndex` prev of
                                       Just ii -> (ii,i)
                                       Nothing -> go (i+1) (prev ++ [c]) cs
                  go _ _ _ = error "unreachable"
                  (cycleStart,cycleRepeat) = go 0 [] cycles
                  cycleSize = cycleRepeat - cycleStart
                  billionth = cycleStart + (1000000000-cycleStart) `mod` cycleSize


    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2