main = do
    let loop is_true i n
          | is_true == True = do
              print i
              loop ((i + 1) < n) (i + 1) n
          | is_true /= True = return ()
    loop (1 < 6) 1 6