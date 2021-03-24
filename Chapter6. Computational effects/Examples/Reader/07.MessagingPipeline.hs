import Control.Monad
import Control.Monad.Reader

type Middleware r m a b = a -> ReaderT r m b

-- implementation

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
  { deserializeFn :: String -> Maybe (Envelope a),
    logFn :: Envelope a -> String
  }

deserialize :: Json -> ReaderT (Context a) Maybe (Envelope a)
deserialize json = do
  ctx <- ask
  let envelope = deserializeFn ctx json
  lift envelope

authorize :: Envelope a -> ReaderT (Context a) Maybe (Envelope a)
authorize envelope =
  if authorized
    then return envelope
    else lift Nothing
  where
    authorized = userId envelope == 5

handle :: Envelope a -> ReaderT (Context a) Maybe String
handle envelope = do
  log <- asks logFn
  return $ log envelope

pipelineFn :: Json -> ReaderT (Context a) Maybe String
pipelineFn = deserialize >=> authorize >=> handle

--test the pipeline
json :: Json
json = "Envelope {userId = 5, payload = ContractCreated {documentId = 1, siteId = 1}}"

ctx :: Context ContractCreated
ctx = Context deserializeFn' logFn'
  where
    deserializeFn' = Just . read
    logFn' = show . payload

result :: Maybe String
result = runReaderT (pipelineFn json) ctx
