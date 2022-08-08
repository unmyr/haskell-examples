import Data.Char (isLower, isUpper)

decodeRot13 :: String -> String
decodeRot13 s = map (decodeRot13Char) s 
  where rotNum = 13
        numOfAlpha = 26 -- Number of letters in the alphabet
        offset c1 c2 = (fromEnum c1) - (fromEnum c2)
        decodeRot13Char c | Data.Char.isLower c =
          toEnum (fromEnum 'a' + ((offset c 'a') + rotNum) `mod` numOfAlpha) :: Char
        decodeRot13Char c | Data.Char.isUpper c =
          toEnum (fromEnum 'A' + ((offset c 'A') + rotNum) `mod` numOfAlpha) :: Char
        decodeRot13Char c = c

main :: IO ()
main = do
  putStrLn $ show ((decodeRot13 "") == "")
  putStrLn $ show ((decodeRot13 "Lbh penpxrq gur pbqr!") == "You cracked the code!")
