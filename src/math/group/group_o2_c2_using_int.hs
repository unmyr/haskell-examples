c2_e :: Int
c2_e = 0

c2_op :: Int -> Int -> Int
-- c2_op a b = (a + b) `mod` 2
c2_op 0 0 = 0
c2_op 1 1 = 0
c2_op 0 1 = 1
c2_op 1 0 = 1
c2_op _ _ = error "ERROR invalid arguments."

c2_inv :: Int -> Int
c2_inv x = head [y | y <- [0, 1], c2_op x y == c2_e]

c2_eval :: [Int] -> [Int]
c2_eval [] = []
c2_eval [x] = [x]
c2_eval [x, y] = (c2_op x y):[]
c2_eval (x:y:zs) = c2_eval ((c2_eval [x, y]) ++ zs)

main :: IO ()
main = do
  putStrLn $ "0^(-1) = " ++ show (c2_inv 0)
  putStrLn $ "1^(-1) = " ++ show (c2_inv 1)
  putStrLn $ "1 ⊕ 1 ⊕ 1 = " ++ show (head (c2_eval [1, 1, 1]))
  putStrLn $ "1 ⊕ 1 ⊕ 1 ⊕ 1 = " ++ show (head (c2_eval [1, 1, 1, 1]))
