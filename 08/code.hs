import Data.List
import Data.Char

charsToNum :: [Char] -> [Int]
charsToNum = map (ord)

maxElem :: (Num a, Ord a) => [a] -> a
maxElem [] = 0
maxElem xs = head . reverse $ sort xs

numVisibleLeft :: [Int] -> Int -> Bool -> Int
numVisibleLeft xs tree include_block
   | vis_trees /= (reverse left_trees) && include_block = 1 + length vis_trees
   | otherwise = length vis_trees
  where vis_trees = takeWhile (\x -> (treeHeight > x)) (reverse left_trees)
        left_trees = (take tree xs)
        treeHeight = xs !! tree
        has_close_tree = (tree > 0)

visibleLeft :: [Int] -> Int -> Bool
visibleLeft xs tree = (length (take tree xs)) == (numVisibleLeft xs tree False)

visibleRight :: [Int] -> Int -> Bool
visibleRight xs tree = visibleLeft (reverse xs) new_tree
   where new_tree = ((length xs) - 1 - tree)

numVisibleRight :: [Int] -> Int -> Bool -> Int
numVisibleRight xs tree include_block = numVisibleLeft (reverse xs) new_tree include_block
   where new_tree = ((length xs) - 1 - tree)

visibleRow :: [Int] -> Int -> Bool
visibleRow xs tree = (visibleLeft xs tree) || (visibleRight xs tree)

isTreeVisible :: Int -> [[Int]] -> Bool
isTreeVisible treeNum forest = (visibleRow (forest !! treeY) treeX) || (visibleRow ((transpose forest) !! treeX) treeY)
  where forestY = length (forest)
        forestX = length (head forest)
        treeX = mod treeNum (forestX)
        treeY = div treeNum forestX

loopThroughTrees :: Int -> [[Int]] -> Int
loopThroughTrees n forest
   | n >= forest_size = 0
   | isTreeVisible n forest = 1 + next_num
   | otherwise = next_num
  where forest_size = (length forest)*(length (head forest))
        next_num = loopThroughTrees (n+1) forest

part1 :: [[Int]] -> Int
part1 xs = loopThroughTrees 0 xs
   where
       forestY = length (xs)
       forestX = length (head xs)

scenicRow :: [Int] -> Int -> Int
scenicRow xs tree = (numVisibleLeft xs tree True) * (numVisibleRight xs tree True)

getScenicScore :: Int -> [[Int]] -> Int
getScenicScore treeNum forest = (scenicRow (forest !! treeY) treeX) * (scenicRow ((transpose forest) !! treeX) treeY)
  where forestY = length (forest)
        forestX = length (head forest)
        treeX = mod treeNum (forestX)
        treeY = div treeNum forestX

getAllScenicScores :: Int -> [[Int]] -> [Int]
getAllScenicScores n forest
   | n >= forest_size = []
   | otherwise = getScenicScore n forest : (getAllScenicScores (n+1) forest)
  where forest_size = (length forest)*(length (head forest))

part2 :: [[Int]] -> Int
part2 xs = maxElem $ getAllScenicScores 0 xs

solve :: [[Int]] -> String
solve i = "Part1 : " ++ (show $ part1 i) ++ "\nPart2 : " ++ (show $ part2 i)

main = interact $ solve . map charsToNum . lines
