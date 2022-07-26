remove2ndElement :: [a] -> [a]
remove2ndElement [] = []
remove2ndElement [x] = [x]
remove2ndElement (x:_:xs) = x:xs

main :: IO ()
main = do
  print $ remove2ndElement ([] :: [Int])
  print $ remove2ndElement [1]
  print $ remove2ndElement [1, 2]
  print $ remove2ndElement [1, 2, 3]
  print $ remove2ndElement [1, 2, 3, 4]
