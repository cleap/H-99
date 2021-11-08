module Unit01 where

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
encode :: (Eq a) => [a] -> [(Int,a)]
-- Human-readable version:
-- encode xs = map helper $ pack xs where
--    helper :: [a] -> (Int, a)
--    helper xs = (myLength xs, head xs)
--
-- From here, I converted encode into pointfree style, following
-- https://stackoverflow.com/a/29596461
--
-- encode = \xs -> map (\ys -> (myLength ys, head ys)) $ pack xs
-- encode = \xs -> (map (\ys -> (myLength ys, head ys)) $ (pack xs)
-- encode = (map (\ys -> (myLength ys, head ys))) . pack
-- encode = (map (\ys -> (,) (myLength ys) (head ys))) . pack
-- encode = (map ((\xs -> (,) (myLength xs)) <*> (\xs -> head xs))) . pack
-- encode = (map ((\xs -> (,) (myLength xs)) <*> head)) . pack
-- encode = (map (((,) . myLength) <*> head)) . pack
-- encode = map (((,) . myLength) <*> head) . pack
--
fork :: (a -> b) -> (b -> c -> d) -> (a -> c) -> a -> d
-- fork f g h x = g (f x) (h x)
-- fork f g h = (g . f) <*> h
-- fork f g h = (<*>) (g . f) h
-- fork f g = (<*>) (g . f)
-- fork f g = (<*>) ((.) g f)
-- fork f = (<*>) . (flip (.) f)
-- fork f = (.) (<*>) (flip (.) f)
-- fork f = (.) (<*>) . (flip (.))
fork = (.) (<*>) . flip (.)

encode = map (fork myLength (,) head) . pack
