import Data.List

data Entry = File Int String
data Directory = None | Dir Directory [Entry] [Directory] String

splitWhen :: Eq a => a -> [a] -> [[a]]
splitWhen c xs = [(takeWhile func xs)]++[(tail $ dropWhile func xs)]
  where func = (\x -> (x /= c))

combinePath :: String -> String -> String
combinePath a b
   | (head b) == '/' = b
   | otherwise = a++b

updatedParent :: Directory -> Directory
updatedParent dir = n_parent
   where (Dir parent _ _ _) = dir
         (Dir p e d n) = parent
         n_parent = (Dir p e (d++[dir]) n)

commandParse :: Directory -> String -> Directory
commandParse dir ('$':' ':'c':'d':' ':'/':[]) = Dir None [] [] "/"
commandParse dir ('$':' ':'c':'d':' ':'.':'.':[]) = updatedParent dir
commandParse dir ('$':' ':'c':'d':' ':xs) = ret_dir
   where ret_dir = Dir dir [] [] xs

doLs :: Directory -> [String] -> (Directory, [String])
doLs cur xs = (ret_dir, rest)
   where ret_dir = (Dir parent files [] name)
         entries = takeWhile (\x -> ((head x) /= '$')) xs
         files = [(File (read $ (words x) !! 0) ((words x) !! 1)) | x <- entries, (head x /= 'd')]
         rest = dropWhile (\x -> ((head x) /= '$')) xs
         (Dir parent _ _ name) = cur

commandLoop :: Directory -> [String] -> Directory
commandLoop c [] = c
commandLoop cur ("$ ls":xs) = commandLoop dir rest
  where (dir, rest) = doLs cur xs
commandLoop cur (x:[]) = commandParse cur x
commandLoop cur (x:xs) = commandLoop cur_dir xs
   where cur_dir = commandParse cur x

findRoot :: Directory -> Directory
findRoot d | n == "/" = d
           | otherwise = findRoot (updatedParent d)
   where (Dir _ _ _ n) = d

createOrphans :: Directory -> Directory
createOrphans (Dir _ e d n) = (Dir None e new_d n)
   where new_d = map createOrphans d

getEntrySize :: Entry -> Int
getEntrySize (File i _) = i

getChildSize :: Directory -> Int
getChildSize (Dir _ e d s) = sum (map getEntrySize e) + (sum children)
   where children = map getChildSize d

createDirList :: Directory -> [Int]
createDirList (Dir _ e d n) = s:(concat $ map createDirList d)
 where s = getChildSize (Dir None e d n)

findFit :: [Int] -> Int -> Int
findFit xs c = head $ sort [x | x <- xs, x >= needed]
  where curFree = (70000000-c)
        needed = (30000000-curFree)

part2 :: [String] -> Int
part2 s = findFit (createDirList $ createOrphans root) (getChildSize root)
  where root = findRoot . commandLoop (None) $ s

filterList :: [Int] -> [Int]
filterList xs = [x | x <- xs, x <= 100000]

part1 :: [String] -> Int
part1 = sum . filterList . createDirList . createOrphans . findRoot . commandLoop (None)

solve :: [String] -> String
solve i = "Part1 : " ++ (show $ part1 i) ++ "\nPart2 : " ++ (show $ part2 i)

main = interact $ solve . lines
