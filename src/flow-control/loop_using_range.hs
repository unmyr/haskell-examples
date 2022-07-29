main :: IO ()
main = do
  let loop i n = case (i < n) of {
    False -> return ();
    True -> do
      print i
      loop (i + 1) n
  }
  loop (1 :: Int) 6
