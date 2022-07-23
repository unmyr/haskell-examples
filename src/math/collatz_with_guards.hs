collatz :: Integer -> [Integer]
collatz n | n == 1         = [1]
          | n `mod` 2 == 0 = [n] ++ collatz (n `div` 2)
          | otherwise      = [n] ++ collatz (n * 3 + 1)

main :: IO ()
main = do
  print $ collatz 9
