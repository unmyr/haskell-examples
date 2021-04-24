digitCount :: Integer -> Int
digitCount = digitCountHelper 1 . abs
  where
    digitCountHelper ds n =
      if n >= 10 then
        digitCountHelper (ds + 1) (n `div` 10)
      else
        ds