notZeroIf :: Int -> Maybe Int
notZeroIf n = do
  if n == 0 then
    Nothing
  else
    Just n

notZeroCase :: Int -> Maybe Int
notZeroCase n = do
  case n == 0 of
    True -> Nothing
    False -> Just n