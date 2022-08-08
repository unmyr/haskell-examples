import Data.Char (chr, isLower, isUpper, ord)

decodeRot13 :: String -> String
decodeRot13 s = map rot13Char s 
  where
    rot13Char c | isLower c = chr . (+ (ord 'a')) $ (`mod` 26) $ (+ 13) $ (+ (- ord 'a')) . ord $ c 
    rot13Char c | isUpper c = chr . (+ (ord 'A')) $ (`mod` 26) $ (+ 13) $ (+ (- ord 'A')) . ord $ c
    rot13Char c = c

main :: IO ()
main = do
  putStrLn $ show ((decodeRot13 "") == "")
  putStrLn $ show ((decodeRot13 "Lbh penpxrq gur pbqr!") == "You cracked the code!")
