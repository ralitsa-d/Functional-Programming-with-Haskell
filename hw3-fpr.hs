import Data.Char
import Data.List

--zad1
normalize :: [Char] -> [Char]
--normalize message = [toUpper x | x <- message, ((x>='a' && x<='z') || (x>='A' && x<='Z'))]
normalize message = normalizeHelper message 0 [] where
 len = length message
 normalizeHelper message count result
  | count == len = result
  | (count /= len) && ((x>='a' && x<='z') || (x>='A' && x<='Z')) = 
     normalizeHelper (tail message) (count + 1) (result ++ [toUpper x])
  | (x >='0' && x <='9') = error "ERROR! NO DIGITS ALLOWED"
  | otherwise = normalizeHelper (tail message) (count + 1) result
    where 
    x = head message

--zad 2a
find_index :: [Char] -> Char -> Int -> Int
find_index alphabet ch count 
 | head alphabet /= ch = find_index (tail alphabet) ch count+1
 | otherwise = count

split :: [Char] -> Char -> ([Char], [Char])
split alphabet ch = splitAt (find_index alphabet ch 0) alphabet

findChar :: [Char] -> Int -> [Char]
findChar alphabet index = snd (splitAt index alphabet) 

findFirstChar :: [Char] -> Char
findFirstChar alphabet = head alphabet

encode :: [Char]-> Char -> Int -> Char
encode alphabet ch offset 
 | ((find_index alphabet ch 0) + offset) >= 0 = findFirstChar (findChar alphabet (rem ((find_index alphabet ch 0) + offset) (length alphabet)))
 | otherwise = findFirstChar (findChar alphabet ((length alphabet) + ((find_index alphabet ch 0) + offset)))

--zad 2b
helper :: [Char] -> [Char] -> [Char]
helper str lst = lst++str
 
encrypt :: [Char] -> Int -> [Char] -> [Char]
encrypt alphabet offset normalized = helper [encode alphabet x offset | x <- normalized] []

--zad 2c
decrypt :: [Char] -> Int -> [Char] -> [Char]
decrypt alphabet offset encrypted = encrypt alphabet (0-offset) encrypted

--zad 3a
crackall :: [Char] -> [Char] -> [[Char]]
crackall alphabet encrypted = map (\x -> decrypt alphabet x encrypted) [0..25]

--zad 3b
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

--zad 3c
findCommonWords :: [[Char]] -> [Char] -> [[Char]]
findCommonWords commonwords message = filter (\x -> substring x message) commonwords

crackcandidates :: [Char] -> [[Char]] -> [Char] -> [[Char]]
crackcandidates alphabet commonwords encrypted = filter (\x -> (findCommonWords commonwords x) /= []) messages where
 messages = crackall alphabet encrypted
 
--zad 4a
polyencrypt :: [Char] -> Int -> Int -> Int -> [Char] -> [Char]
polyencrypt alphabet offset step blockSize normalized  = polyencryptHelper alphabet offset step blockSize normalized 0 [] where
 polyencryptHelper alphabet offset step blockSize normalized count result
  | count == 0 = 
    polyencryptHelper alphabet offset step blockSize (drop blockSize normalized) (count + 1) (result ++ (map (\ch -> encode alphabet ch offset) (take blockSize normalized)))
  | length normalized <= blockSize = 
    result ++ (map (\ch -> encode alphabet ch (offset + step)) normalized)
  | length normalized > blockSize = 
    polyencryptHelper alphabet (offset + step) step blockSize (drop blockSize normalized) (count + 1) (result ++ (map (\ch -> encode alphabet ch (offset + step)) (take blockSize normalized)))

--zad 4b
polydecrypt :: [Char] -> Int -> Int -> Int -> [Char] -> [Char]
polydecrypt alphabet offset step blockSize normalized  = polydecryptHelper alphabet offset step blockSize normalized 0 [] where
 polydecryptHelper alphabet offset step blockSize normalized count result
  | count == 0 = 
    polydecryptHelper alphabet (offset - step) step blockSize (drop blockSize normalized) (count + 1) (result ++ (map (\ch -> encode alphabet ch (0 - offset)) (take blockSize normalized)))
  | length normalized <= blockSize = 
    result ++ (map (\ch -> encode alphabet ch (offset - step)) normalized)
  | length normalized > blockSize = 
    polydecryptHelper alphabet (offset - step) step blockSize (drop blockSize normalized) (count + 1) (result ++ (map (\ch -> encode alphabet ch (offset - step)) (take blockSize normalized)))

--zad 5a
enigmaencrypt :: [Char] -> [Int] -> [Char] -> [Char]
enigmaencrypt alphabet rotors normalized = polyencrypt alphabet (head rotors) (head (tail rotors)) (last rotors) normalized 

--zad 5b
enigmadecrypt :: [Char] -> [Int] -> [Char] -> [Char]
enigmadecrypt alphabet rotors normalized = polydecrypt alphabet (head rotors) (head (tail rotors)) (last rotors) normalized












