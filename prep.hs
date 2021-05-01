isPrefix :: [Char] -> [Char] -> Bool
isPrefix [] str = True
isPrefix sub [] = False
isPrefix (x:xs) (y:ys) = (x == y) && isPrefix xs ys

substring :: [Char] -> [Char] -> Bool
substring [] str = True
substring sub [] = False
substring sub str 
 |isPrefix sub str = True
 |otherwise = substring sub (tail str)

substrings :: [Char] -> [[Char]] -> [[Char]]
substrings [] _ = []
substrings _ [] = []
substrings str lst = filter (\x -> substring x str) lst

createAlphabetChecker :: String -> (String -> Bool)
createAlphabetChecker strToCheck = (\x -> (foldr and (elem x str)) strToCheck)