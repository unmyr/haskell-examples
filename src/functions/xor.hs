import Debug.Trace

xor :: (Integral a, Show a) => [a] -> [a]
xor [] = trace("DEBUG[]") []
xor [x] = trace("DEBUG[" ++ show x ++ "]") $ x:[]
xor [x, y] =
  trace("DEBUG[" ++ show x ++ "," ++ show y ++ "]") $
  ((x + y) `mod` 2):[]
xor (x:y:zs) =
  trace ("DEBUG(" ++ show x ++ ":" ++ show y ++ ":" ++ show zs ++ ")") $
  xor (xor [x, y] ++ zs)

main :: IO ()
main = do
  print $ xor ([] :: [Int]) == []
  print $ xor ([1, 1] :: [Int]) == [0]
  print $ xor ([1, 1, 1] :: [Int]) == [1]
  print $ xor ([1, 1, 1, 1] :: [Int]) == [0]
