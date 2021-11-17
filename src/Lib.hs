module Lib where

-- Problem 1 --
myLast :: [a] -> a
myLast [] = error "empty list"
myLast (x:[]) = x
myLast (x:xs) = myLast xs

-- Problem 2 --
myButLast :: [a] -> a
myButLast [] = error "empty list"
myButLast (x:[]) = error "single element list"
myButLast (x:y:[]) = x
myButLast (x:xs) = myButLast xs

-- Problem 3 ---
elementAt :: [a] -> Int -> a
elementAt [] _ = error "index out of bounds"
elementAt (x:xs) n
    | n <  1    = error "index out of bounds"
    | n == 1    = x
    | otherwise = elementAt xs (n - 1)

-- Problem 4 --
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- Problem 5 --
myReverse :: [a] -> [a]
myReverse xs = helper [] xs where
    helper :: [a] -> [a] -> [a]
    helper xs [] = xs
    helper xs (y:ys) = helper (y:xs) ys

-- Problem 6 --
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome = (==) <*> myReverse

-- Problem 7 --
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem x) = [x]
flatten (List (x:xs)) = (flatten x) ++ (flatten (List xs))

-- Problem 8 --
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:[]) = [x]
compress (x1:x2:xs)
    | x1 == x2  = compress (x1:xs)
    | otherwise = x1 : (compress (x2:xs))

-- Problem 9 --
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:[]) = [[x]]
pack (x:xs) = helper [x] xs where
    helper :: (Eq a) => [a] -> [a] -> [[a]]
    helper xs [] = [xs]
    helper xs (y:ys)
        | (head xs) == y = helper (y:xs) ys
        | otherwise      = xs:(helper [y] ys)

-- Problem 10 --
fork :: (a -> b) -> (b -> c -> d) -> (a -> c) -> a -> d
fork = (.) (<*>) . flip (.)
encode :: (Eq a) => [a] -> [(Int,a)]
encode = map (fork myLength (,) head) . pack

-- Problem 11 --
data EncodeItem a = Single a | Multiple Int a deriving (Eq, Show)
encodeModified :: (Eq a) => [a] -> [EncodeItem a]
encodeModified xs = map helper $ pack xs where
    helper :: (Eq a) => [a] -> EncodeItem a
    helper [x] = Single x
    helper xs = Multiple (myLength xs) (head xs)

-- Problem 12 --
decodeModified :: [EncodeItem a] -> [a]
decodeModified [] = []
decodeModified (Single x:xs) = x : decodeModified xs
decodeModified (Multiple n x:xs) = replicate n x ++ decodeModified xs

-- Problem 13 --
encodeDirect :: (Eq a) => [a] -> [EncodeItem a]
encodeDirect [] = []
encodeDirect (x:xs) = helper (Single x) xs where
    helper :: (Eq a) => EncodeItem a -> [a] -> [EncodeItem a]
    helper x [] = [x]
    helper (Single y) (x:xs)
        | x == y = helper (Multiple 2 x) xs
        | otherwise = Single y : helper (Single x) xs
    helper (Multiple n y) (x:xs)
        | x == y = helper (Multiple (n + 1) x) xs
        | otherwise = Multiple n y : helper (Single x) xs

-- Problem 14 --
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : (x : dupli xs)

-- Problem 15 -- 
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = helper x n ++ repli xs n where
    helper :: a -> Int -> [a]
    helper a 0 = []
    helper a n = a : helper a (n - 1)

-- Problem 16 --
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = helper xs n n where
    helper :: [a] -> Int -> Int -> [a]
    helper [] _ _ = []
    helper (x:xs) 1 n = helper xs n n
    helper (x:xs) i n = x : helper xs (i - 1) n

-- Problem 17 --
split :: [a] -> Int -> ([a],[a])
split [] _ = error "Empty list"
split xs n = helper [] n xs where
    helper :: [a] -> Int -> [a] -> ([a],[a])
    helper _ _ [] = error "Index out of bounds"
    helper xs 0 ys = (xs,ys)
    helper xs n (y:ys) = helper (xs ++ [y]) (n - 1) ys

-- Problem 18 --
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice xs i j = trimEnd [] (trimFront xs i) (j - i) where
    trimFront :: [a] -> Int -> [a]
    trimFront [] _ = error "Index out of bounds"
    trimFront xs 1 = xs
    trimFront (x:xs) i = trimFront xs (i - 1)
    trimEnd :: [a] -> [a] -> Int -> [a]
    trimEnd _ [] _ = error "Index out of bounds"
    trimEnd xs (y:ys) 0 = xs ++ [y]
    trimEnd xs (y:ys) j = trimEnd (xs ++ [y]) ys (j - 1)

-- Problem 19 --
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs n =
    let helper :: [a] -> [a] -> Int -> [a]
        helper _ [] _ = error "Index out of bounds"
        helper xs ys 0 = ys ++ xs
        helper xs (y:ys) n = helper (xs ++ [y]) ys (n-1)
    in  if n <  0 then helper [] xs (length xs + n)
        else helper [] xs n

-- Problem 20 --
removeAt :: Int -> [a] -> (a,[a])
removeAt i xs = helper i [] xs where
    helper :: Int -> [a] -> [a] -> (a,[a])
    helper _ _ [] = error "Empty list"
    helper 1 xs (y:ys) = (y,xs++ys)
    helper i xs (y:ys) = helper (i - 1) (xs++[y]) ys
