module RandomAccessList
    ( someFunc
    ) where

type Nat = Word

-- Random Access List
data Tree a = Leaf a | Node Nat (Tree a) (Tree a) deriving Show

size :: Tree a -> Nat
size (Leaf x) = 1
size (Node n _ _ ) = n

data Digit a = Zero | One (Tree a) deriving Show
type RAList a = [Digit a]

fromRA::RAList a -> [a]
fromRA = concatMap from
    where
        from Zero =[]
        from (One t) = fromT t

fromT :: Tree a -> [a]
fromT (Leaf x) = [x]
fromT (Node _ t1 t2) = fromT t1 ++ fromT t2

node ::Tree a -> Tree a -> Tree a
node t1 t2 = Node (size t1 +size t2) t1 t2

fetchRA:: Nat -> RAList a -> a
fetchRA k (Zero: xs) = fetchRA k xs
fetchRA k (One t: xs) =
    if k < size t
    then fetchT k t
    else fetchRA (k - size t) xs

fetchT :: Nat -> Tree a -> a
fetchT 0 (Leaf x) = x
fetchT k (Node n t1 t2) =
    if k < m then fetchT k t1
    else fetchT (k - m) t2
    where m = n `div` 2

someFunc :: String -> IO()
someFunc = putStrLn

consRA:: a -> RAList a -> RAList a
consRA x xs = consT (Leaf x) xs
consT t1 [] = [One t1]
consT t1 (Zero: xs) = One t1: xs
consT t1 (One t2: xs) = Zero: consT (node t1 t2) xs

