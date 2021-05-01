import Data.Char

integer :: Int
integer = 1+2

mystery :: Int -> Int -> Int
mystery x y 
 | x > y = y
 | otherwise = x
 
iSort :: [Int] -> [Int]
iSort [] = []
iSort (x:xs) = ins x (iSort xs)

ins :: Int -> [Int] -> [Int]
ins x [] = [x]
ins x (y:ys)
 | x <= y = x:(y:ys)
 | otherwise = y : ins x ys
 
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
 |x<=y = x:merge xs (y:ys)
 |otherwise = y:merge (x:xs) ys
 
mergeSort :: Ord a => [a] -> [a]
mergeSort arr
 |len<2 = arr
 |otherwise = merge (mergeSort rs) (mergeSort ls) where
 len = length arr
 (rs, ls) = splitAt (len `div` 2) arr

try :: Integer -> Integer -> Integer
try x y
 | x<y = x
 | otherwise = error "Error"

{-isPrefix :: String -> String -> Bool
isPrefix [] ys = True
isPrefix (x:xs) [] = False
isPrefix (x:xs) (y:ys) = (x == y) && isPrefix xs ys

strContains :: String -> String -> Bool
strContains (x:xs) [] = False
strContains xs ys
  | isPrefix xs ys = True
  | strContains xs (tail ys) = True
  | otherwise = False-}
  
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
 
 
isVowel :: Char -> Bool
isVowel c = elem c "aeiouy"

countVowels :: String -> String
countVowels str = filter isVowel str

countVs :: String -> Int
countVs str = length (filter isVowel str)






















  
  
  
  
