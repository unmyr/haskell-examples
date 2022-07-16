nthRoot :: Int -> Int -> [(Int)]
nthRoot n k = [ x | x <- [2..(n `div` 2) + 1], x^k == n]