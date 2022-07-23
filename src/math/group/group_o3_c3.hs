c3_e = 0
c3_all = [0, 1, 2]

c3_op x y = (x + y) `mod` 3
c3_inv x = head [y | y <- c3_all, c3_op x y == c3_e]

c3 :: [Int] -> [Int]
c3 [] = []
c3 [x] = [x]
c3 (x:xs) = c3 ([c3_op x (head xs)] ++ (tail xs))

main = do
  print $ [c3_op 0  y | y <- c3_all]
  print $ [c3_op 1  y | y <- c3_all]
  print $ [c3_op 2  y | y <- c3_all]
  putStrLn $ "0^(-1) = " ++ show (c3_inv 0)
  putStrLn $ "1^(-1) = " ++ show (c3_inv 1)
  putStrLn $ "1 * 2 = " ++ show (head (c3 [1, 2]))
  putStrLn $ "1 * 2 * 1 = " ++ show (head (c3 [1, 2, 1]))
