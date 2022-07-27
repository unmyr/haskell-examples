swapString :: (String, String) -> (String, String)
swapString (a, b) = (b, a)

swap :: (a, a) -> (a, a)
swap (a, b) = (b, a)

main :: IO ()
main = do
  print $ swapString ("hello", "world") == ("world", "hello")
  print $ swap ("hello", "world") == ("world", "hello")
  print $ swap(swap("hello", "world")) == ("hello", "world")
  print $ (swap . swap)("hello", "world") == ("hello", "world")
  print $ (swap $ swap("hello", "world") == ("hello", "world"))
  print $ swap (1 :: Int, 2) == (2, 1)
