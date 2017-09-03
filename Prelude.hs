map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = f x : map' xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ []     = []
filter' p (x:xs)
    | p x        = x : filter' xs
    | otherwise  = filter' xs

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

max' :: (Ord a) => a -> a -> a
max' x y
    | x > y     = x
    | otherwise = y

maximum' :: (Ord a) => [a] -> a
maximum' []     = error "List must have at least one element."
maximum' [x]    = x
maximum' (x:xs) = max' x $ maximum' xs

last' :: [a] -> a
last' []     = error "List must have at least one element."
last' [x]    = x
last' (x:xs) = last' xs

replicate' :: Int -> a -> [a]
replicate' n x
    | n < 0     = error "Cannot create a list of an element a negative number of times."
    | n == 0    = []
    | otherwise = x : replicate' (n - 1) x

take' :: (Num a, Ord a) => a -> [b] -> [b]
take' _ []      = []
take' n (x:xs)
    | n < 0     = error "Cannot take a negative number of elements from a list."
    | n == 0    = []
    | otherwise = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ []      = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = a `elem'` xs

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _          = []
zip' _ []          = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _          = []
zipWith' _ _ []          = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
