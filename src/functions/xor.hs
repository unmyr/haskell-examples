import Debug.Trace

xor :: (Integral a, Show a) => [a] -> [a]
xor [] = trace("DEBUG[]") []
xor [x] = trace("DEBUG[" ++ show x ++ "]") [x]
xor (x:y:zs) =
  trace ("DEBUG(" ++ show x ++ ":" ++ show y ++ ":" ++ show zs ++ ")")
  xor ([(x + y `mod` 2) `mod` 2] ++ zs)

main :: IO ()
main = do
  print $ xor ([] :: [Int]) == []
  print $ xor ([1, 1, 1, 1] :: [Int]) == [0]
