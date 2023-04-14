module BinarySearch (search) where

type Nat = Int

search ::(Nat -> Nat) -> Nat -> [Nat]
search f t = seek (0,t)
    where
        seek (a,b)  | a>b =[]
                    | t <f m = seek (a,m − 1)
                    | t == f m = [m]
                    | otherwise = seek (m+1,b)
                    where m = choose (a,b)
