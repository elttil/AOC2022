didWin :: String -> String -> Bool
didWin a b
   | a == "A" = (b == "Y")
   | a == "B" = (b == "Z")
   | a == "C" = (b == "X")
   | otherwise = False

pickScore :: String -> Int
pickScore "X" = 1
pickScore "Y" = 2
pickScore "Z" = 3
pickScore "A" = 1
pickScore "B" = 2
pickScore "C" = 3

pickWin :: String -> String
pickWin a
   | a == "A" = "Y"
   | a == "B" = "Z"
   | a == "C" = "X"

pickLoser :: String -> String
pickLoser a
   | a == "A" = "Z"
   | a == "B" = "X"
   | a == "C" = "Y"

pickDraw :: String -> String
pickDraw a
   | a == "A" = "X"
   | a == "B" = "Y"
   | a == "C" = "Z"

getPick :: String -> String -> String
getPick a b 
   | b == "X" = pickLoser a
   | b == "Y" = pickDraw a
   | b == "Z" = pickWin a

didDraw :: String -> String -> Bool
didDraw a b = (pickScore a) == (pickScore b)

playRound :: String -> String -> Int
playRound a b | didWin a b = 6 + pickScore b
              | didDraw a b = 3 + pickScore b
              | otherwise = pickScore b

proxyFunction :: String -> String -> Int
proxyFunction a b = playRound a (getPick a b)

part1 :: [String] -> Int
part1 [] = 0
part1 (a:b:xs) = playRound a b + part1 xs

part2 :: [String] -> Int
part2 [] = 0
part2 (a:b:xs) = proxyFunction a b + part2 xs

solve :: [String] -> String
solve i = "Part1 : " ++ (show $ part1 i) ++ "\nPart2 : " ++ (show $ part2 i)

main = interact $ solve . words
