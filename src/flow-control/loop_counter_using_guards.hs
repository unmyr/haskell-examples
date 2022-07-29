main :: IO ()
main = do
  let loop i
       | i <= 5 = do
         print i
         loop (i + 1)
       | otherwise = return ()
  loop (1 :: Int)
