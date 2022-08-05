main :: IO ()
main = do
  putStrLn "-- head ([1..] :: [Int])"
  print $ head ([1..] :: [Int])
  putStrLn "-- (\\(x:_) -> x) ([1..] :: [Int])"
  print $ (\(x:_) -> x) ([1..] :: [Int])
  putStrLn "-- ([1..] :: [Int]) !! 0"
  print $ ([1..] :: [Int]) !! 0
  putStrLn "-- take 1 $ ([1..] :: [Int])"
  print $ take 1 $ ([1..] :: [Int])
  putStrLn "-- foldr (\\x _ -> x:[]) [] ([1..] :: [Int])"
  print $ foldr (\x _ -> x:[]) [] ([1..] :: [Int])
