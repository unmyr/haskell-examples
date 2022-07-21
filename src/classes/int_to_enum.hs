data Fruits = Apple | Orange | Kiwi deriving (Enum, Show)

main = do
    print (toEnum 0 :: Fruits)
    print (toEnum 1 :: Fruits)
    print (toEnum 2 :: Fruits)
    print (map toEnum [0, 1, 2] :: [Fruits])
