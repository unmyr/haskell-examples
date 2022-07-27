data C2 = C2 Int

instance Eq C2 where
  (C2 x) == (C2 x') = x == x'

instance Show C2 where
  show a = case a of
    C2 0 -> "0"
    C2 1 -> "1"
    _    -> "?"

instance Monoid C2 where
  mempty = C2 0
  mappend (C2 x) (C2 y) = C2 ((x + y) `mod` 2)

c2_inv :: C2 -> C2
c2_inv (C2 x) = head [C2 y | y <- [0..2], (C2 x) `mappend` (C2 y) == (mempty)]

c2_eval :: [C2] -> [C2]
c2_eval [] = []
c2_eval [x] = [x]
c2_eval [x, y] = (x `mappend` y):[]
c2_eval (x:y:zs) = c2_eval ((c2_eval [x, y]) ++ zs)

main :: IO ()
main = do
  putStrLn $ "0^(-1) = " ++ show (c2_inv (C2 0))
  putStrLn $ "1^(-1) = " ++ show (c2_inv (C2 1))
  putStrLn $ "1 ⊕ 1 ⊕ 1 = " ++ show (head (c2_eval [(C2 1), (C2 1), (C2 1)]))
  putStrLn $ "1 ⊕ 1 ⊕ 1 ⊕ 1 = " ++ show (head (c2_eval [(C2 1), (C2 1), (C2 1), (C2 1)]))
