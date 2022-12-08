splitWhen :: Eq a => a -> [a] -> [[a]]
splitWhen c xs = [(takeWhile func xs)]++[(tail $ dropWhile func xs)]
  where func = (\x -> (x /= c))

parseLine :: String -> [Int]
parseLine i = map read $ concat $ map (splitWhen '-') (splitWhen ',' i)

overlaps :: [Int] -> Bool
overlaps (a:b:x:y:_) = ((a <= y) && (x <= a)) || ((x <= b) && (a <= x))

contains :: [Int] -> Bool
contains (a:b:x:y:_) = ((a <= x) && (b >= y)) || (x <= a) && (y >= b)

part2 :: [String] -> Int
part2 [] = 0
part2 (x:xs) | overlaps (parseLine x) = 1 + part2 xs
             | otherwise = part2 xs

part1 :: [String] -> Int
part1 [] = 0
part1 (x:xs) | contains (parseLine x) = 1 + part1 xs
             | otherwise = part1 xs

solve :: [String] -> String
solve i = "Part1 : " ++ (show $ part1 i) ++ "\nPart2 : " ++ (show $ part2 i)

main = interact $ solve . lines
