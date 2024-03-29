import Debug.Trace
import Data.Monoid

traceMyFoldrL :: (Show a, Show b) => (a -> b -> b) -> b -> [a] -> b
traceMyFoldrL _ z [] =
  trace("traceMyFoldrL _ " ++ (show z) ++ " [] = " ++ (show z))
  $ z
traceMyFoldrL f z (x:xs) =
  trace("traceMyFoldrL f " ++ (show z) ++ " (" ++ (show x) ++ ":" ++ (show xs) ++ ") = f " ++ (show x) ++ " (traceMyFoldrL f " ++ (show z) ++ " " ++ (show xs) ++ ")" )
  $ f x (traceMyFoldrL f z xs)

traceMyFoldrF :: (Foldable t, Show a, Show b) => (a -> b -> b) -> b -> t a -> b
traceMyFoldrF f z t =
  trace("traceMyFoldrF f " ++ (show z) ++ " " ++ "t" ++ " = appEndo (foldMap (Endo . f) t) z " ++ (show z) ++ " " ++ "t")
  $ appEndo (foldMap (Endo . f) t) z

traceMyFoldlL :: (Show a, Show b) => (b -> a -> b) -> b -> [a] -> b
traceMyFoldlL _ z [] =
  trace("traceMyFoldlL f " ++ (show z) ++ " [] = " ++ (show z))
  $ z
traceMyFoldlL f z (x:xs) =
  trace("traceMyFoldlL f " ++ (show z) ++ " (" ++ (show x) ++ ":" ++ (show xs) ++ ") = traceMyFoldlL f (f " ++ (show z) ++ " " ++ (show x) ++ ") " ++ (show xs))
  $ traceMyFoldlL f (f z x) xs

traceMyFoldlF :: (Foldable t, Show a, Show b) => (b -> a -> b) -> b -> t a -> b
traceMyFoldlF f z t =
  trace("traceMyFoldlF f " ++ (show z) ++ "t = appEndo (getDual (foldMap (Dual . Endo . flip f) t)) " ++ (show z))
  $ appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z

traceOp :: Int -> Int -> Int
traceOp x y = trace("traceOp(x - y) = " ++ (show x) ++ " - (" ++ (show y) ++ ")") $ x - y

main :: IO ()
main = do
  putStrLn "-- foldr traceOp 0 ([1, 2, 3] :: [Int])"
  print $ foldr traceOp 0 ([1, 2, 3] :: [Int])
  putStrLn "-- traceMyFoldrL traceOp 0 ([1, 2, 3] :: [Int])"
  print $ traceMyFoldrL traceOp 0 ([1, 2, 3] :: [Int])
  putStrLn "-- traceMyFoldrF traceOp 0 ([1, 2, 3] :: [Int])"
  print $ traceMyFoldrF traceOp 0 ([1, 2, 3] :: [Int])

  putStrLn "-- foldl traceOp 0 ([1, 2, 3] :: [Int])"
  print $ foldl traceOp 0 ([1, 2, 3] :: [Int])

  putStrLn "-- traceMyFoldlL traceOp 0 ([1, 2, 3] :: [Int])"
  print $ traceMyFoldlL traceOp 0 ([1, 2, 3] :: [Int])
  putStrLn "-- traceMyFoldlF traceOp 0 ([1, 2, 3] :: [Int])"
  print $ traceMyFoldlF traceOp 0 ([1, 2, 3] :: [Int])
