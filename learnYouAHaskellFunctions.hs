import Data.List (group, sort)

wordNums :: String -> [(String,Int)]
wordNums = map (\xs -> (head xs, length xs)) . group . sort . words
