import Data.Char (chr, ord)

decodeRot13 :: String -> String
decodeRot13 s = map rot13Char s
  where
    rot13Char c | 'a' <= c && c <= 'm' = chr $ (+   13 ) $ ord c
    rot13Char c | 'n' <= c && c <= 'z' = chr $ (+ (-13)) $ ord c
    rot13Char c | 'A' <= c && c <= 'M' = chr $ (+   13 ) $ ord c
    rot13Char c | 'N' <= c && c <= 'Z' = chr $ (+ (-13)) $ ord c
    rot13Char c = c

main :: IO ()
main = do
  putStrLn $ show ((decodeRot13 "") == "")
  putStrLn $ show ((decodeRot13 "Lbh penpxrq gur pbqr!") == "You cracked the code!")
