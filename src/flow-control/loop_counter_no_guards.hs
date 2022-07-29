main :: IO ()
main = do
  let loop 6 = return ()
      loop i = do
        print i
        loop (i + 1)
  loop (1 ::Int)
