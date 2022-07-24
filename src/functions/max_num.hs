import Data.Int

-- Get maximum number
max_num :: (Num a, Ord a) => a -> a -> a
max_num x y = if x > y then x else y

-- Get maximum element
max_gen :: (Ord a) => a -> a -> a
max_gen x y = if x > y then x else y

main :: IO ()
main = do
  print $ max_num (3 :: Data.Int.Int8) 4 == 4
  print $ max_num (3 :: Int) 4           == 4
  print $ max_num (3 :: Integer) 4       == 4
  print $ max_num (1/3 ::Rational) 1/2   == 1/2
  print $ max_num (pi::Float)  (sqrt(2)::Float)  == 3.1415927
  print $ max_num (pi::Double) (sqrt(2)::Double) == 3.141592653589793
  print $ max_gen (3 :: Int) 4
  print $ max_gen (1/3 ::Rational) 1/2   == 1/2
  print $ max_gen (pi::Float) (sqrt(2)::Float)   == 3.1415927
  print $ max_gen (pi::Double) (sqrt(2)::Double) == 3.141592653589793
  print $ max_gen True False == True
  print $ max_gen 'z' 'a' == 'z'
  print $ max_gen "apple" "kiwi" == "kiwi"
