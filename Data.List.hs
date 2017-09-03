import Data.List (tails)

nub' :: [a] -> [a]
nub' [] = []
nub' (x:xs) = x : nub' $ filter (/= x) xs

isInfixOf' :: (Eq a) => a -> a -> Bool
isInfixOf' xs = foldr1 (&&) . filter (isPrefixOf xs) . tails
