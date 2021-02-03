factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n -1)

maybeValue :: Maybe Integer
maybeValue = Just 5

ioValue :: IO Integer
ioValue = do
  putStrLn "give me a number pls"
  read <$> getLine


