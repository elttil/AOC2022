import Data.List

splitWhen :: Eq a => a -> [a] -> [[a]]
splitWhen c xs = [(takeWhile func xs)]++[(tail $ dropWhile func xs)]
  where func = (\x -> (x /= c))

removeWhitespace :: String -> String
removeWhitespace xs = [x | x <- xs, x /= ' ']

cleanupCrates :: [String] -> [String]
cleanupCrates [] = []
cleanupCrates (x:xs)
   | length x == 0 = cleanupCrates xs
   | elem '[' x = cleanupCrates xs
   | elem ']' x = cleanupCrates xs
   | elem ',' x = cleanupCrates xs
   | otherwise = x:cleanupCrates xs

parseMove :: String -> [Int]
parseMove ('m':'o':'v':'e':' ':xs) = (read num) : parseMove rest
   where (num:rest:_) = splitWhen ' ' xs
parseMove ('f':'r':'o':'m':' ':b:' ':'t':'o':' ':c:_) = (map read [[b], [c]])

parse s = (crates, moves)
  where splitstring = splitWhen "" $ lines s
        crates = map init . cleanupCrates . map (removeWhitespace) . transpose $ head splitstring
        moves = map parseMove $ last splitstring

insertIntoListPart1 :: Int -> [a] -> [[a]] -> [[a]]
insertIntoListPart1 index object list = (take (index-1) list) ++ [new_entry] ++ (drop (index) list)
   where new_entry = (reverse object) ++ (list !! (index-1))

insertIntoListPart2 :: Int -> [a] -> [[a]] -> [[a]]
insertIntoListPart2 index object list = (take (index-1) list) ++ [new_entry] ++ (drop (index) list)
   where new_entry = (object) ++ (list !! (index-1))

removeFromList :: Int -> Int -> [[a]] -> [[a]]
removeFromList index amount list = (take (index-1) list) ++ [new_entry] ++ (drop (index) list)
   where new_entry = (drop amount (list !! (index-1)))

performMovePart1 :: ([String], [[Int]]) -> ([String], [[Int]])
performMovePart1 (x, []) = (x, [])
performMovePart1 i = performMovePart1 (new_crates, (tail moves))
   where (crates, moves) = i
         move = head moves
         (amount:from:to:_) = move
         new = (take amount (crates !! (from-1)))
         new_crates = removeFromList from amount $ insertIntoListPart1 to new crates

performMovePart2 :: ([String], [[Int]]) -> ([String], [[Int]])
performMovePart2 (x, []) = (x, [])
performMovePart2 i = performMovePart2 (new_crates, (tail moves))
   where (crates, moves) = i
         move = head moves
         (amount:from:to:_) = move
         new = (take amount (crates !! (from-1)))
         new_crates = removeFromList from amount $ insertIntoListPart2 to new crates

part1 :: String -> String
part1 = map head . fst . performMovePart1 . parse

part2 :: String -> String
part2 = map head . fst . performMovePart2 . parse

solve :: String -> String
solve i = "Part1 : " ++ (part1 i) ++ "\nPart2 : " ++ (part2 i)

main = interact $ solve
