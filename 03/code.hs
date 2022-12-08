
import Data.List
import Data.Char

splitHalf l = splitAt ((length l + 1) `div` 2) l


mutuals :: Eq a => [a] -> [a] -> [a]
mutuals []       _                = []
mutuals (x : xs) ys | x `elem` ys = x : mutuals xs (delete x ys)
                    | otherwise   = mutuals xs ys

getPriority :: String -> Int
getPriority (c:_)
   | (c >= 'a' && c <= 'z') = (ord c) - (ord 'a') + 1
   | (c >= 'A' && c <= 'Z') = (ord c) - (ord 'A') + 27

sharedChar :: [String] -> String
sharedChar (x:y:[]) = (mutuals x y)
sharedChar (x:y:z:_) = mutuals (mutuals x y) z
sharedChar (x:y:xs) = (sharedChar ((mutuals x y):xs))

findCommon :: [String] -> Int
findCommon s = getPriority (sharedChar (s))

part2 :: [String] -> Int
part2 [] = 0
part2 (x:y:z:xs) = (findCommon (x:y:[z])) + part2 xs

part1 :: [String] -> Int
part1 [] = 0
part1 (x:xs) = (findCommon (a:[b])) + part1 xs
   where (a, b) = splitHalf x

solve :: [String] -> String
solve i = "Part1 : " ++ (show $ part1 i) ++ "\nPart2 : " ++ (show $ part2 i)

main = interact $ solve . words
