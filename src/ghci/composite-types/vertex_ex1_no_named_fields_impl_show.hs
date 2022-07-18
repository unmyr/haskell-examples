data Vertex = Vertex Int Int
instance Show Vertex where
  show (Vertex x y) = "Vertex " ++ show x ++ " " ++ show y
