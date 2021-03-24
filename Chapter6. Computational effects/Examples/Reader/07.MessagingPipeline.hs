import Control.Monad
import Control.Monad.Reader

type Middleware r m a b = a -> ReaderT r m b

type UserId = Int

type Json = String

data ContractCreated = ContractCreated
  { documentId :: Int,
    siteId :: Int
  }
  deriving (Show, Read)

data Envelope a = Envelope
  { userId :: UserId,
    payload :: a
  }
  deriving (Show, Read)

data Context a = Context
  { authorizeFn :: UserId -> Bool,
    deserializeFn :: String -> Maybe (Envelope a),
    logFn :: Envelope a -> String
  }

deserialize :: Json -> ReaderT (Context a) Maybe (Envelope a)
deserialize json = do
  ctx <- ask
  let envelope = deserializeFn ctx json
  lift envelope

authorize :: Envelope a -> ReaderT (Context a) Maybe (Envelope a)
authorize envelope = do
  ctx <- ask
  let authorized = authorizeFn ctx (userId envelope)
  if authorized
    then return envelope
    else lift Nothing

handle :: Envelope a -> ReaderT (Context a) Maybe String
handle envelope = do
  log <- asks logFn
  return $ log envelope

pipelineFn :: Json -> ReaderT (Context a) Maybe String
pipelineFn = deserialize >=> authorize >=> handle

authorizeFn' :: UserId -> Bool
authorizeFn' = (== 5)

deserializeFn' :: (Read a) => String -> Maybe (Envelope a)
deserializeFn' = Just . read

logFn' :: (Show a) => Envelope a -> String
logFn' = show . payload


json :: Json
json = "Envelope {userId = 5, payload = ContractCreated {documentId = 1, siteId = 1}}"

ctx :: Context ContractCreated
ctx = Context authorizeFn' deserializeFn' logFn'

result :: Maybe String
result = runReaderT (pipelineFn json) ctx
