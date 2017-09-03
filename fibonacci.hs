fibonacci :: (Int a) => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fib (n - 1) + fib (n - 2)
