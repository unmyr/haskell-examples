import Data.Char (isLower, isUpper)

decodeRot13 :: String -> String
decodeRot13 s = map rot13Char s
  where
    makePairsRot13 cl = zip cl ((drop 13 cl) ++ (take 13 cl))
    lowerPairs = makePairsRot13 ['a'..'z'] -- [('a','n'),('b','o'), ...,('z','m')]
    upperPairs = makePairsRot13 ['A'..'Z'] -- [('A','N'),('B','O'), ...,('Z','M')]
    rot13Char c | isLower c = snd $ lowerPairs !! (fromEnum c - fromEnum 'a')
    rot13Char c | isUpper c = snd $ upperPairs !! (fromEnum c - fromEnum 'A')
    rot13Char c = c

main :: IO ()
main = do
  putStrLn $ show ((decodeRot13 "") == "")
  putStrLn $ show ((decodeRot13 "Lbh penpxrq gur pbqr!") == "You cracked the code!")
