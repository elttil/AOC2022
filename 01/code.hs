import Data.List

sumList :: [String] -> Int
sumList [] = 0
sumList (x:xs) = (read x :: Int) + sumList xs

sumGroups :: [[String]] -> [Int]
sumGroups [] = []
sumGroups (x:xs) = sumList x : sumGroups xs

groupNums :: [String] -> [[String]]
groupNums [] = [[]]
groupNums xs | length second == 0 = [first]
             | otherwise = [first] ++ groupNums (tail second)
   where first = takeWhile (\x -> (x /= "")) xs
         second = dropWhile (\x -> (x /= "")) xs

part1 :: [String] -> Int
part1 = maximum . sumGroups . groupNums

part2 :: [String] -> Int
part2 i = sum $ take 3 (reverse . sort . sumGroups $ groupNums i)

solve :: [String] -> String
solve i = "Part1 : " ++ (show $ part1 i) ++ "\nPart2 : " ++ (show $ part2 i)

main = interact $ solve . lines
