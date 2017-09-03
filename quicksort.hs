-- Two different implementations of quicksort.
-- Both are very short, and differ only in style.

-- | Using List Comprehensions
quicksort :: (Ord a) => [a] -> [a]
quicksort (x:xs) = quicksort smaller ++ [x] ++ quicksort larger
  where
    smaller = [e | e <- xs, e <= x]
    larger  = [e | e <- xs, e > x]

-- | Using the filter function
quicksort :: (Ord a) => [a] -> [a]
quicksort (x:xs) = quicksort smaller ++ [x] ++ quicksort larger
  where
    smaller = filter (<= x) xs
    smaller = filter (> x) xs
