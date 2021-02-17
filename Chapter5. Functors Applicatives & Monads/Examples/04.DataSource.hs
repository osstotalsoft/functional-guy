import Control.Monad

--Define the DataSource Algebra
type DataSource i o = i -> DataSourceResult o

newtype DataSourceResult a = DataSourceResult
  { run :: IO (Maybe a)
  }

instance Functor DataSourceResult where
  fmap fn (DataSourceResult r) = DataSourceResult (fmap (fmap fn) r)

instance Applicative DataSourceResult where
  pure = DataSourceResult . pure . pure
  (DataSourceResult fn) <*> (DataSourceResult x) = DataSourceResult r
    where
      r =
        do
          fn' <- fn
          x' <- x
          return $ fn' <*> x'

instance Monad DataSourceResult where
  (DataSourceResult x) >>= fn = DataSourceResult r
    where
      r =
        do
          x' <- x
          run $ fn' x'

      fn' Nothing = DataSourceResult (return Nothing)
      fn' (Just x) = fn x

--Use the DataSource algebra in some domain

type UserId = Integer

type ContractId = Integer

type ClientId = Integer

type AssetId = Integer

type Amount = Double

data Contract = Contract
  { contractId :: ContractId,
    clientId :: ClientId,
    amount :: Amount
  }
  deriving (Show)

data Asset = Asset
  { assetId :: AssetId,
    ctrId :: ContractId,
    description :: String
  }
  deriving (Show)

--primitive DataSources
contractsByClientId :: DataSource ClientId [Contract]
contractsByClientId clientId =
  DataSourceResult
    ( do
        putStrLn $ "Executing contractsByClientId sql query for clientId " ++ show clientId
        --return Nothing
        --ioError $ userError "Some Sql exc"
        let contracts = [Contract 1 clientId 2000.0, Contract 2 clientId 2000.0]
        print contracts
        return $ Just contracts
    )

assetsByContractId :: DataSource ContractId [Asset]
assetsByContractId contractId =
  DataSourceResult
    ( do
        putStrLn $ "Executing assetsByContractId sql query for contractId " ++ show contractId
        let assets = [Asset 1 contractId "BMW E92", Asset 1 contractId "BMW E60"]
        print assets
        return $ Just assets
    )

clientIdByUserId :: DataSource UserId ClientId
clientIdByUserId userId =
  DataSourceResult
    ( do
        putStrLn $ "Executing clientIdByUserId sql query for userId " ++ show userId
        let clientId = 10 + userId
        print clientId
        return $ Just clientId
    )

--create new data sources by combining existing ones
totalContractsAmountByClientId :: DataSource ClientId Amount
totalContractsAmountByClientId clientId = totalAmount <$> contractsByClientId clientId
  where
    totalAmount = foldr ((+) . amount) 0

totalContractsAmountByUserid :: DataSource UserId Amount
totalContractsAmountByUserid = clientIdByUserId >=> totalContractsAmountByClientId

assetsByClientId :: DataSource ClientId [Asset]
assetsByClientId clientId = do
  contracts <- contractsByClientId clientId
  assets <- mapM (assetsByContractId . contractId) contracts
  return $ join assets
