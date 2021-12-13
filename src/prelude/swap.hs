swap :: String -> String -> (String, String)
swap a b = (b, a)
main = do
    let (a, b) = swap "hello" "world"
    putStrLn (a ++ " " ++ b)
