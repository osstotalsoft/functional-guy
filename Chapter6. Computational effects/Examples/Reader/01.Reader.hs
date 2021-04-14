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

--example 1
data Config = Config {topicPrefix :: String, natsUrl :: String}

data CorrelationId = NoCorrelationId

data Event = NoEvent

data Command = NoCommand

sendCommand :: Command -> Reader Config CorrelationId
sendCommand cmd = do
  config <- ask
  let prefix = topicPrefix config
  let url = natsUrl config
  let correlationId = NoCorrelationId
  return correlationId

waitForEvent :: CorrelationId -> Reader Config Event
waitForEvent corelationId = do
  config <- ask
  let prefix = topicPrefix config
  let url = natsUrl config
  let event = NoEvent
  return event

sendCommandAndWaitForEvent :: Command -> Reader Config Event
sendCommandAndWaitForEvent = sendCommand >=> waitForEvent



--example2
data Env = Env {isProd :: Bool, secret :: Integer}

isProdEnv :: Reader Env Bool
isProdEnv = isProd <$> ask

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



data Context = NA

myFn:: Int -> String
myFn _ = ""

myOtherFn:: String -> Double
myOtherFn _ = 11.3

myComposedFn = myOtherFn . myFn


myFn':: Int -> (Context -> String)
myFn' _ _ = ""

myOtherFn':: String -> (Context -> Double)
myOtherFn' _ _= 11.3

--myComposedFn' = myOtherFn' . myFn'

myComposedFn' :: Int -> Context -> Double
myComposedFn' = myFn' >=> myOtherFn'
