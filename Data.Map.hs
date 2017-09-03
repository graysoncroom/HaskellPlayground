-- The following three functions are different implementations of "lookup"
-- These functions are almost identical and differ only in style.

lookup1' :: (Eq k) => k -> [(k,v)] -> Maybe v
lookup1' _ []    = Nothing
lookup1' k (x:xs)
    | k = key   = Just value
    | otherwise = lookup1' k xs
  where
    key   = fst x
    value = snd x

lookup2' :: (Eq k) => k -> [(k,v)] -> Maybe v
lookup2' _ [] = Nothing
lookup2' k ((key, value):xs)
    | k == key = Just value
    | otherwise = lookup2' k xs

lookup3' :: (Eq k) => k -> [(k,v)] -> Maybe v
lookup3' key = foldr (\(k,v) acc -> if k == key then Just v else acc) Nothing
