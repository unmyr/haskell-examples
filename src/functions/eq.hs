eq :: (Eq a) => a -> a -> Bool
eq x y = if x == y then True else False

main :: IO ()
main = do
  print $ eq (3 :: Int) 3
  print $ eq (1/3 :: Rational) (1/3 :: Rational)
  print $ eq (pi::Float) 3.1415927
  print $ eq (pi::Double) 3.141592653589793
  print $ eq 'a' 'a'
  print $ eq "hello" "hello"
  print $ eq (1, 2) (1, 2)
