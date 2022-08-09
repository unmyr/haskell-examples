import Data.Char (isLower, isUpper)

data LetterCase = LowerCase | UpperCase | NonAlphabeticCharacter deriving Show

checkCase :: Char -> LetterCase
checkCase c = case c of
  ch | 'a' <= ch && ch <= 'z' -> LowerCase
  ch | 'A' <= ch && ch <= 'Z' -> UpperCase
  _ -> NonAlphabeticCharacter

checkCase' :: Char -> LetterCase
checkCase' c = case c of
  ch | isLower ch -> LowerCase
  ch | isUpper ch -> UpperCase
  _ -> NonAlphabeticCharacter

main :: IO ()
main = do
  let letters = ['a', 'A', '-']
  putStrLn $ "checkCase : " ++ show(letters) ++ " -> " ++ show (map checkCase  letters)
  putStrLn $ "checkCase': " ++ show(letters) ++ " -> " ++ show (map checkCase' letters)
