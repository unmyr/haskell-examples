reverse' :: [a] -> [a]
reverse' [x] = [x]
reverse' (x:xs) = reverse xs ++ [x]
