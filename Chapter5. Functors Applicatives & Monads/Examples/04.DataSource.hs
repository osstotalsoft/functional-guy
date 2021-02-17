newtype DataSourceResult a = DataSourceResult (IO (Maybe a))

newtype DataSource i o = DataSource
  { run :: i -> IO (Maybe o)
  }

instance Functor (DataSource i) where
  fmap fn ds = DataSource (fmap (fmap fn) . run ds)

instance Applicative (DataSource i) where
  --pure x = DataSource (\_ -> pure (pure x))
  pure = DataSource . const . pure . pure
  fn <*> x =
    DataSource
      ( \i ->
          do
            fn' <- run fn i
            x' <- run x i
            return $ fn' <*> x'
      )

instance Monad (DataSource i) where
  a >>= fn =
    DataSource
      (\i -> run a i >>= (\a' -> run (fn' a') i))
    where
      fn' Nothing = DataSource (\_ -> return Nothing)
      fn' (Just x) = fn x

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

type UserId = Int

type TenantId = Int

defaultTenantId :: DataSource UserId TenantId
defaultTenantId = return 1

getUserNameForTenantId :: TenantId -> DataSource UserId String
getUserNameForTenantId tenantId =
  DataSource
    (\userId -> return $ Just $ show tenantId ++ "_" ++ show userId)

defaultTenantUserName :: DataSource UserId String
defaultTenantUserName = defaultTenantId >>= getUserNameForTenantId