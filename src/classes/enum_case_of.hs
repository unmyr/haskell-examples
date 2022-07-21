import Data.List

data Fruits = Apple | Orange | Kiwi deriving (Enum, Show)

talk :: Fruits -> String 
talk sym = case sym of
  Apple -> "I like apples."
  Orange -> "I like oranges."
  Kiwi -> "I like kiwis."

main = do
  let fruits = [Apple, Orange, Kiwi]
  putStrLn
    $ Data.List.intercalate "\n"
    $ map (\sym -> show sym ++ ": " ++ talk sym) fruits
