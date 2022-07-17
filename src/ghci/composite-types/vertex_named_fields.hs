data Vertex = Vertex { x :: Int, y :: Int }
instance Show Vertex where
  show (Vertex x y) = "Vertex (" ++ show x ++ "," ++ show y ++ ")"

main = do
  let a = Vertex 3 4
      b = Vertex 2 3
  print a
  print $ x a
  print (y a)
  print $ (\v -> sqrt(fromIntegral (x v * x v + y v * y v))) a
  print b
