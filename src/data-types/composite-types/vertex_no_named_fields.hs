data Vertex = Vertex Int Int deriving (Show)

class DistanceSpace a where
  distance :: (a, a) -> Float

instance DistanceSpace Vertex where
  distance (v1, v2) =
    sqrt(fromIntegral(
      ((\(Vertex x _) -> x) v2 - (\(Vertex x _) -> x) v1)^2 +
      ((\(Vertex _ y) -> y) v2 - (\(Vertex _ y) -> y) v1)^2))

global_scoped_distance :: (Vertex, Vertex) -> Float
global_scoped_distance (v1, v2) =
  sqrt(fromIntegral(
    ((\(Vertex x _) -> x) v2 - (\(Vertex x _) -> x) v1)^2 +
    ((\(Vertex _ y) -> y) v2 - (\(Vertex _ y) -> y) v1)^2))

main = do
  let a = Vertex 3 4
      b = Vertex 2 3
  print a
  print $ (\(Vertex x _) -> x) a
  print $ (\(Vertex _ y) -> y) a
  print $ (\(Vertex x y) -> sqrt (fromIntegral (x*x + y*y))) a
  print b
