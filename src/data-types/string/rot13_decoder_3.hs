decodeRot13 :: String -> String
decodeRot13 s = map rot13Char s
  where
    -- [('a','n'),('b','o'), ...,('z','m')]
    lowerMap = zip ['a'..'z'] (take (length ['a'..'z']) (drop 13 (cycle ['a'..'z'])))
    -- [('A','N'),('B','O'), ...,('Z','M')]
    upperMap = zip ['A'..'Z'] (take (length ['A'..'Z']) (drop 13 (cycle ['A'..'Z'])))
    alphaMap = lowerMap ++ upperMap
    rot13Char c = case lookup c alphaMap of
        Just value -> value
        Nothing -> c

main :: IO ()
main = do
  putStrLn $ show ((decodeRot13 "") == "")
  putStrLn $ show ((decodeRot13 "Lbh penpxrq gur pbqr!") == "You cracked the code!")
