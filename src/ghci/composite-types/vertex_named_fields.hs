data Vertex = Vertex { vertex_x :: Int, vertex_y :: Int }
instance Show Vertex where
  show (Vertex x y) = "Vertex {x=" ++ show x ++ ",y=" ++ show y ++ "}"

class DistanceSpace a where
  distance :: (a, a) -> Float

instance DistanceSpace Vertex where
  distance (v1, v2) =
    sqrt(fromIntegral ((vertex_x v2 - vertex_x v1)^2 + (vertex_y v2 - vertex_y v1)^2))

global_scoped_distance :: (Vertex, Vertex) -> Float
global_scoped_distance (v1, v2) =
    sqrt(fromIntegral ((vertex_x v2 - vertex_x v1)^2 + (vertex_y v2 - vertex_y v1)^2))

main = do
  let a = Vertex 3 4
      b = Vertex 2 3
  print a
  print $ vertex_x a
  print (vertex_y a)
  print $ (\v -> sqrt(fromIntegral (vertex_x v ^2 + vertex_y v ^2))) a
  print b
