newtype DataSource i o = DataSource
  { run :: i -> IO (Maybe o)
  }

mapDs :: (a -> b) -> DataSource i a -> DataSource i b
mapDs fn ds = DataSource (fmap (fmap fn) . run ds)

instance Functor (DataSource i) where
  fmap = mapDs

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n -1)

ds1 :: DataSource Integer Integer
ds1 =
  DataSource
    ( \id -> do
        putStrLn "Doing some sql query"
        return $ Just id
    )

ds2 :: DataSource Integer [Integer]
ds2 =
  DataSource
    ( \id -> do
        putStrLn "Doing some sql query"
        return $ Just [1 .. id]
    )

ds3 :: DataSource Integer String
ds3 =
  DataSource
    ( \_ -> do
        putStrLn "Doing some sql query"
        return Nothing
    )

x :: DataSource Integer Integer
x = factorial <$> ds1

y :: DataSource Integer [Integer]
y = fmap (fmap factorial) ds2

z :: DataSource Integer Int
z = length <$> ds3
