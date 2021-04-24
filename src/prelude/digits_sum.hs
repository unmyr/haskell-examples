digitSum :: Int -> Int
digitSum = digitSumHelper 0 . abs
  where
    digitSumHelper ds n =
      if n >= 10 then
        digitSumHelper (ds + (n `mod` 10)) (n `div` 10)
      else
        ds + n