-- algebraic data types
data StateOfMatter = Gas | Liquid | Solid | Plasma -- so far it's like an enum

--recursive algebraic types
data Tree a = Nil | Node a (Tree a) (Tree a)
    deriving(Eq, Show, Read)

tree :: Tree Integer
tree = (Node 17
        (Node 14 Nil Nil)
        (Node 20 
            (Node 19 Nil Nil) 
            Nil))
            
secondTree :: Tree Integer
secondTree = (Node 17
        (Node 14 Nil Nil)
        (Node 20 
            (Node 19 Nil Nil) 
            Nil))

stringTree :: Tree String
stringTree = (Node "ani"
                (Node "haskell" Nil Nil)
                (Node "ne"
                    (Node "ni6to" Nil Nil)
                    Nil))

-- calcultes the depth of a tree
depth :: Tree a -> Integer
depth Nil = 0
depth (Node n l r) = 1 + max (depth l) (depth r)

-- says if two trees are the same
equal :: Eq a => Tree a -> Tree a -> Bool
equal Nil Nil = True
equal Nil _ = False
equal _ Nil = False
equal (Node n1 l1 r1) (Node n2 l2 r2)
 | n1 == n2 = equal l1 l2 && equal r1 r2
 | otherwise = False

--traverses a tree (left -> root -> right) and turns it to a list
toList :: Tree a -> [a]
toList Nil = []
toList (Node n l r) = (toList l) ++ [n] ++ (toList r)


isBST :: Ord a => Tree a -> Bool
isBST t = isSorted (toList t)

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted (x:xs)
 | xs == [] = True
 | x <= head xs = isSorted xs
 | otherwise = False

example :: String -> String 
example xs = filter (\x -> elem x ['a'..'z']) xs

