import Data.Char
import Data.List

--zad 1
digits :: Integer -> [Integer]
digits n
 | n < 10    = [n]
 | otherwise = digits (n `div` 10) ++ [n `mod` 10]
 
newArr :: [Integer] -> Integer -> [Integer] -> [Integer]
newArr [] count result = result
newArr (x:lst) count result = if (rem count 2)==0 then newArr lst (count+1) [x*2]++result else newArr lst (count+1) [x]++result

sumDigits :: [Integer] -> Integer
sumDigits xs = foldr1 (+) (map (\x -> (foldr1 (+) (digits x))) xs)

calcLuhnChecksum :: Integer -> Integer
calcLuhnChecksum n = rem ((sumDigits (newArr (digits n) 1 [])) * 9) 10

--zad2
data BTree = Empty | Node Int BTree BTree
 deriving (Eq, Show, Read)
 
bt :: BTree
bt = Node 0 (Node 1 (Node 3 (Node 7 Empty Empty)
 Empty)
 (Node 4 (Node 8 Empty Empty)
 Empty))
 (Node 2 (Node 5 (Node 9 Empty Empty)
 (Node 10 Empty Empty))
 (Node 6 Empty
 (Node 11 Empty Empty)))
 
depth :: BTree -> Int
depth Empty = 0
depth (Node _ lt rt) = 1 + max (depth lt) (depth rt)

stringSize :: BTree -> Int
stringSize t = 2^(depth t)-1

onKLevel :: BTree -> Int -> String
onKLevel Empty _ = []
--onKLevel (Node null Empty Empty) _ = ['_']
onKLevel (Node n lt rt) 1 = [intToDigit n]
onKLevel (Node _ lt rt) k = (onKLevel lt (k-1)) ++ (onKLevel rt (k-1))

findNumberOf_ :: Int -> Int -> Int -> Int
findNumberOf_ level countLevels strsize 
 |countLevels /= level = findNumberOf_ level (countLevels+1) (div strsize 2)
 |otherwise = div strsize 2
 {-|countLevels /= level = last(map (\x -> strsize = strsize/2) [1..countLevels])-}
 
make :: BTree -> String -> Int -> [[Char]]
make btree [] level = []
make btree (x:onklevel) level = [replicate (findNumberOf_ level 1 size) '_'] ++ 
   [[x]] ++ [replicate (findNumberOf_ level 1 size) '_'] ++ [['_']] ++ (make btree onklevel level) 
   where
   size = stringSize btree

makeRow :: BTree -> String -> Int -> [Char]
makeRow btree onklevel level = concat (init (make btree onklevel level))

numberOfLevels :: BTree -> Int -> Int
numberOfLevels btree k = if (onKLevel btree k) =="" then k else numberOfLevels btree k

makeRows :: BTree -> Int -> [Char]
makeRows btree 5 = []
makeRows btree level = (makeRow btree (onKLevel btree level) level)++ "\n" ++ (makeRows btree (level+1))


printBT :: BTree -> [Char]
printBT btree = makeRows btree 1


















