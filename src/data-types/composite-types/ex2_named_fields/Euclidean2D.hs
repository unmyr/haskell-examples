module Euclidean2D where
data Vertex = Vertex { x :: Int, y :: Int }
instance Show Vertex where
  show (Vertex x y) = "Vertex {x=" ++ show x ++ ",y=" ++ show y ++ "}"
