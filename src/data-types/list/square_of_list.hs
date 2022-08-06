import Debug.Trace

sqList :: (Integral t) => [t] -> [t]
sqList [] = []
sqList (x:xs) = (x * x):sqList(xs)

sqListMonad :: Int -> [Int]
sqListMonad n = do
  value <- [1 .. n]
  return (value^(2::Int))

traceMyFoldrLH :: (Show a, Show b) => (a -> b -> b) -> b -> [a] -> b
traceMyFoldrLH _ z [] =
  trace("traceMyFoldrLH f " ++ (show z) ++ " [] = " ++ (show z))
  $ z
traceMyFoldrLH f z (x:xs) =
  trace("traceMyFoldrLH f " ++ (show z) ++ " (" ++ (show x) ++ ":[..]) = f " ++ (show x) ++ " (traceMyFoldrLH f " ++ (show z) ++ " [..])")
  $ f x (traceMyFoldrLH f z xs)

traceMyFoldlLH :: (Show a, Show b) => (b -> a -> b) -> b -> [a] -> b
traceMyFoldlLH _ z [] =
  trace("traceMyFoldlLH f " ++ (show z) ++ " [] = " ++ (show z))
  $ z
traceMyFoldlLH f z (x:xs) =
  trace("traceMyFoldlLH f " ++ (show z) ++ " (" ++ (show x) ++ ":[..]) = traceMyFoldlLH f (f " ++ (show z) ++ " " ++ (show x) ++ ") [..]")
  $ traceMyFoldlLH f (f z x) xs

main :: IO ()
main = do
  -- Using recursive function
  putStrLn "-- take 5 $ sqList ([1..] :: [Int])"
  print $ take 5 $ sqList ([1..] :: [Int])

  -- Using `map`
  putStrLn "\n-- take 5 $ map (\\x -> x * x) ([1..] :: [Int])"
  print $ take 5 $ map (\x -> x * x) ([1..] :: [Int])
  putStrLn "\n-- take 5 $ map (^(2::Int)) ([1..] :: [Int])"
  print $ take 5 $ map (^(2::Int)) ([1..] :: [Int])

  -- Using `fmap`
  putStrLn "\n-- take 5 $ fmap (\\x -> x * x) ([1..] :: [Int])"
  print $ take 5 $ fmap (\x -> x * x) ([1..] :: [Int])
  -- using an infix synonym(<$>) for `Data.Functor.fmap`.
  putStrLn "\n-- take 5 $ (\\x -> x * x) <$> ([1..] :: [Int])"
  print $ take 5 $ (\x -> x * x) <$> ([1..] :: [Int])

  -- using list comprehension
  putStrLn "\n-- take 5 $ [x * x | x <- ([1..] :: [Int])]"
  print $ take 5 $ [x * x | x <- ([1..] :: [Int])]

  -- Using `foldr`
  putStrLn "\n-- take 5 $ foldr (\\x xs -> (x * x) : xs) [] ([1..] :: [Int])"
  print $ take 5 $ foldr (\x xs -> (x * x) : xs) [] ([1..] :: [Int])

  -- Applicative 1
  putStrLn "\n-- take 5 $ pure (^(2::Int)) <*> ([1..] :: [Int])"
  print $ take 5 $ pure (^(2::Int)) <*> ([1..] :: [Int])
  -- Applicative 2
  putStrLn "\n-- take 5 $ [(^(2::Int))] <*> ([1..] :: [Int])"
  print $ take 5 $ [(^(2::Int))] <*> ([1..] :: [Int])

  -- With Monad 1
  putStrLn "\n-- take 5 $ ([1..] :: [Int]) >>= \\x -> pure (x * x)"
  print $ take 5 $ ([1..] :: [Int]) >>= \x -> pure (x * x)
  -- With Monad 2
  putStrLn "\n-- take 5 $ do {value <- ([1..] :: [Int]); pure (value ^ (2::Int))}"
  print $ take 5 $ do {value <- ([1..] :: [Int]); pure (value ^ (2::Int))}
  -- With Monad 3
  putStrLn "\n-- sqListMonad 5"
  print $ sqListMonad 5
  -- With Monad 4
  putStrLn "\n-- take 5 $ (\\n -> pure (n * n)) =<< ([1..] :: [Int])"
  print $ take 5 $ (\n -> pure (n * n)) =<< ([1..] :: [Int])

  -- foldr
  putStrLn "\n-- traceMyFoldrLH (\\x y -> x) 0 ([1..] :: [Int])"
  print $ traceMyFoldrLH (\x _ -> trace("(\\x y -> x) = " ++ (show x)) $ x) (0 :: Int) ([1..] :: [Int])
  putStrLn "\n-- take 5 $ traceMyFoldrLH (\\x y -> trace(\"(\\x y -> (x * x) : y) = \" ++ (show (x * x)) ++ \":\" ++ show y) $ (x * x) : y) [] ([1..6] :: [Int])"
  print $ take 5 $ traceMyFoldrLH (\x y -> trace("(\\x y -> (x * x) : y) = " ++ (show (x * x)) ++ ":" ++ show y) $ (x * x) : y) [] ([1..6] :: [Int])
  putStrLn "\n-- take 5 $ traceMyFoldrLH (\\x y -> trace(\"(\\x y -> (x * x) : y) = \" ++ (show (x * x)) ++ \":y\") $ (x * x) : y) [] ([1..] :: [Int])"
  print $ take 5 $ traceMyFoldrLH (\x y -> trace("(\\x y -> (x * x) : y) = " ++ (show (x * x)) ++ ":y") $ (x * x) : y) [] ([1..] :: [Int])
