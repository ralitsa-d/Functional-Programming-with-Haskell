import Data.Char
import Data.List

largerthan :: Integer -> [Integer]
largerthan n = n:(largerthan (n+1))

sumThree :: Integer -> Integer -> Integer -> Integer
sumThree i1 i2 i3 = (i1 + i2) + i3

fact :: Integer -> Integer
fact n 
 |n==0 = 1
 |otherwise = n*(fact (n-1))
 
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n*(factorial (n-1))

countDigits :: Integer -> Integer
countDigits n
 |n<10 = 1
 |otherwise = 1+(countDigits (div n 10))
 
digits :: Integer -> [Integer]
digits n
 |n<10 = [n]
 |otherwise = digits (div n 10) ++ [rem n 10]
 
narc :: Integer -> Bool
narc n = n == sum [i^(countDigits n) | i <- ds] where
     ds = digits n

--pattern matching
elem_ :: Int -> [Int] -> Bool
elem_ n [] = False
elem_ n (x:xs) 
 |(n == x) = True 
 |otherwise = (elem_ n xs)

--2 variants of doubleAll
doubleAll :: [Int] -> [Int]
doubleAll [] = []
doubleAll xs = [2*x | x <- xs]

doubleAll_ :: [Int] -> [Int]
doubleAll_ [] = []
doubleAll_ (x:xs) = 2*x : (doubleAll_ xs)

multiply :: Int -> Int -> Int
multiply x y = x*y

doubleAll2 :: [Int] -> [Int]
doubleAll2 = map (multiply 2)

--2 variants of selectEven
isEven :: Int -> Bool
isEven n
 |(rem n 2) == 0 = True
 |otherwise = False

selectEven_ :: [Int] -> [Int] 
selectEven_ xs = [x | x <- xs, isEven x] 

selectEven :: [Int] -> [Int] 
selectEven [] = []
selectEven (x:xs)
 |isEven x = x : (selectEven xs)
 |otherwise = (selectEven xs)

--Sortirane vyv vyzhodqsht red
iSort :: [Int] -> [Int] 
iSort [] = [] 
iSort (x:xs) = ins x (iSort xs) 
 
ins :: Int -> [Int] -> [Int] 
ins x [] = [x] 
ins x (y:ys)    
 | x <= y    = x:(y:ys)    
 | otherwise = y : ins x ys 

take_ :: Int -> [a] -> [a]
take_ n [] = []
take_ 0 (x:xs) = []
take_ n (x:xs) = x : (take (n-1) xs)

{-(++) :: [a] -> [a] -> [a] 
[] ++ ys     = ys 
(x:xs) ++ ys = x:(xs ++ ys) -}

{-foldr1 :: (a -> a -> a) -> [a] -> a 
foldr1 f [x]    = x 
foldr1 f (x:xs) = f x (foldr1 f xs) -}

--ecercises bbsbb
toDigits :: Integer -> [Integer]
toDigits n  = reverse (helper n) where
 helper n
  |n<10 = [n]
  |otherwise = (rem n 10) : (helper (div n 10))
  
myMax :: (Ord a) => a -> a -> a
myMax x y
 |x>y = x
 |y>=x = y
 

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) 
 |x < y = x : (merge xs (y:ys))
 |otherwise = y : (merge (x:xs) ys)

mergesort :: (Ord a, Num a) => [a] -> [a]
mergesort lst
 | (length lst) < 2  = lst
 | otherwise = (merge (mergesort first) (mergesort second))
 where
 first = take mid lst
 second = drop mid lst
 mid = (div (length lst) 2)
 
myDrop :: Int -> [a] -> [a]
myDrop n lst
 |n==(length lst) = []
 |n>(length lst) = error "Error"
 |n==0 = lst
 |otherwise = myDrop (n-1) (tail lst)
 
isOdd :: Int -> Bool
isOdd n = not (isEven n)
 
oddAndEven :: (Integral a) => [a] -> ([a], [a])
oddAndEven lst = ([x | x <- lst, x `mod` 2 == 0], [y | y <- lst, y `mod` 2 == 1])

occurencesMin :: [Int] -> Int
occurencesMin [] = 0
occurencesMin lst = length [x | x <- lst, x == mini]
 where 
 mini = minimum lst

isPrefix :: String -> String -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x:xs) (y:ys) = (x==y) && (isPrefix xs ys)

isSubstring :: String -> String -> Bool
isSubstring [] _ = True
isSubstring _ [] = False
isSubstring sub str
 | isPrefix sub str = True
 | otherwise = isSubstring sub (tail str)

atLeastVowel :: [String] -> Int -> [String]
atLeastVowel xs n = filter (\word -> (checkvowels word) >= n) xs
 where
 checkvowels :: String -> Int
 checkvowels "" = 0
 checkvowels (x:xs) 
  | (elem x "ayouei") = 1+(checkvowels xs)
  | otherwise = (checkvowels xs)

compose :: [(Int -> Int)] -> (Int -> Int)
--compose [] = id
--compose (x:xs) = x . (compose xs)
compose xs = foldr1 (.) xs

--compose [(\x -> x+1), (\x -> x+2), (\x -> x+3)] 1
isFactor :: Int -> Int -> Bool
isFactor a b = (mod a b) == 0

isPrime :: Int -> Bool
isPrime n = null $ (filter (isFactor n) [2..n-1])

mindBlown :: Int -> (Int, Int)
mindBlown n = head (filter (\(x, y) -> isPrime x && isPrime y) (map (\a -> (a, n-a)) [2..(div n 2)]))

presentWords :: String -> [String] -> [String]
presentWords str xs = filter (\word -> isSubstring word str) xs 
 
subtrOddEven :: [Int] -> Int
subtrOddEven xs = (foldr (+) 0 odds) - (foldr (+) 0 evens)
 where
 evens = [x | x <- xs, isEven x]
 odds = [y | y <- xs, isOdd y]
 
madeOf :: String -> (String -> Bool)
madeOf alphabet = and . map (\ch -> (elem ch alphabet))









