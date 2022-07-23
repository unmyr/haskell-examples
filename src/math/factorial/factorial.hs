-- Using pattern matching
fac_p :: (Eq a, Num a) => a -> a
fac_p 0 = 1
fac_p n = n * fac_p (n-1)

-- Using conditional guards
fac_g :: (Eq a, Num a) => a -> a
fac_g n | n == 0    = 1
        | otherwise = n * fac_g (n-1)

-- Using if-then-else
fac_if_then_else :: (Eq a, Num a) => a -> a
fac_if_then_else n =
  if n == 0 then 1 else n * fac_if_then_else (n-1)

main :: IO ()
main = do
  print $ fac_p (42 :: Integer)
  print $ fac_g (42 :: Integer)
  print $ fac_if_then_else (42 :: Integer)
