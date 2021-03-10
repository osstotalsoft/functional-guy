import Control.Monad

newtype Reader e a = Reader {runReader :: e -> a}

instance Functor (Reader e) where
  fmap f ra = Reader (f . runReader ra)

instance Applicative (Reader e) where
  pure = Reader . const
  rf <*> ra =
    Reader
      ( \e ->
          let f = runReader rf e
              a = runReader ra e
           in f a
      )

instance Monad (Reader e) where
  ra >>= k =
    Reader
      ( \e ->
          let a = runReader ra e
              rb = k a
           in runReader rb e
      )

ask :: Reader a a
ask = Reader id

data Env = Env {isProd :: Bool, secret :: Integer}

isProdEnv :: Reader Env Bool
isProdEnv = not . isProd <$> ask

showEnv :: Reader Env String
showEnv = do
  prod <- isProdEnv
  return $ if prod then "PRODUCTION" else "DEVELOPMENT"

increment :: Integer -> Reader Env Integer
increment x = do
  prod <- isProdEnv
  return $ if prod then x + 1 else x -1

double :: Integer -> Reader Env Integer
double x = do
  prod <- isProdEnv
  return $ if prod then x * 2 else x

incrementThenDouble :: Integer -> Reader Env Integer
incrementThenDouble = increment >=> double

incrementThenDoubleTheSecret :: Reader Env Integer
incrementThenDoubleTheSecret = do
  env <- ask
  let x = secret env
  x' <- increment x
  double x'
