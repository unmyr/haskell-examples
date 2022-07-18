data Vertex = Vertex Int Int
instance Show Vertex where
  show (Vertex x y) = "Vertex " ++ show x ++ " " ++ show y

main = do
  let a = Vertex 3 4
      b = Vertex 2 3
  print a
  print $ (\(Vertex x _) -> x) a
  print $ (\(Main.Vertex x _) -> x) a
  print $ (\(Vertex _ y) -> y) a
  print $ (\(Vertex x y) -> sqrt (fromIntegral (x*x + y*y))) a
  print b
