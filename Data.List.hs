nub :: [a] -> [a]
nub [] = []
nub (x:xs) = x : nub $ filter (/= x) xs
