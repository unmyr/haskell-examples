c2_e :: Int
c2_e = 0

c2_op :: Int -> Int -> Int
-- c2_op a b = (a + b) `mod` 2
c2_op 0 0 = 0
c2_op 1 1 = 0
c2_op 0 1 = 1
c2_op 1 0 = 1
c2_op _ _ = error "ERROR invalid arguments."

inv :: Int -> Int
inv x = head [y | y <- [0, 1], c2_op x y == c2_e]

c2 :: [Int] -> [Int]
c2 [] = []
c2 [x] = [x]
c2 (x:xs) = c2 ([c2_op x (head xs)] ++ (tail xs))

main :: IO ()
main = do
  putStrLn $ "0^(-1) = " ++ show (inv 0)
  putStrLn $ "1^(-1) = " ++ show (inv 1)
  putStrLn $ "1 ⊕ 1 ⊕ 1 = " ++ show (head (c2 [1, 1, 1]))
  putStrLn $ "1 ⊕ 1 ⊕ 1 ⊕ 1 = " ++ show (head (c2 [1, 1, 1, 1]))
