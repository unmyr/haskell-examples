import Euclidean2D

main = do
  let a = Vertex {x = 3, y = 4}
      b = Vertex 5 12
  print a
  print $ x a
  print $ Euclidean2D.x a
  print (y a)
  print $ (\v -> sqrt(fromIntegral (x v ^2 + y v ^2))) a
