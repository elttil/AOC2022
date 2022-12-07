import Data.List

allDifferent xs = nub xs == xs

findSec :: Int -> String -> Int
findSec n x | allDifferent (take n x) = n
            | otherwise = 1+(findSec n $ tail x)

part1 :: String -> Int
part1 = findSec 4

part2 :: String -> Int
part2 = findSec 14

solve :: String -> String
solve i = "Part1 : " ++ (show $ part1 i) ++ "\nPart2 : " ++ (show $ part2 i)

main = interact $ solve
