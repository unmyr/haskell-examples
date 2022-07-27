-- Using pattern matching
facRecPm :: (Integral a) => a -> a
facRecPm 0 = 1
facRecPm n = n * facRecPm (n-1)

-- Using conditional guards
facRecCg :: (Integral a, Eq a) => a -> a
facRecCg n | n == 0    = 1
           | otherwise = n * facRecCg (n-1)

-- Using if-then-else
facRecIfThenElse :: (Integral a, Eq a) => a -> a
facRecIfThenElse n =
  if n == 0 then 1 else n * facRecIfThenElse (n-1)

main :: IO ()
main = do
  print $ facRecPm (20 :: Int) == 2432902008176640000
  print $ facRecPm (21 :: Int) == -4249290049419214848
  print $ facRecPm (20 :: Integer) == 2432902008176640000
  print $ facRecCg (21 :: Integer) == 51090942171709440000
  print $ facRecIfThenElse (21 :: Integer) == 51090942171709440000
