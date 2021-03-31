{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Identity
import qualified Data.Map as Map
import Data.Maybe
import Data.Typeable

--schema
type TenantId = Int

type UserId = Int

type ClaimId = Int

type ClaimName = String

data Tenant = Tenant {tenantId :: TenantId, tenantName :: String} deriving (Show)

data User = User {userId :: UserId, userName :: String, _tenantId :: TenantId} deriving (Show)

data Claim = Claim {claimId :: ClaimId, claimName :: String}

data UserClaim = UserClaim {_userId :: UserId, _claimId :: ClaimId}

data Query a where
  GetTenantById :: TenantId -> Query Tenant
  GetUserById :: UserId -> Query User
  GetAllUsers :: Query [User]
  GetClaimById :: ClaimId -> Query Claim
  GetAllUsersByTenantId :: TenantId -> Query [User]
  GetTenantByUserId :: UserId -> Query Tenant
  GetUserHasClaim :: (UserId, ClaimName) -> Query Bool
  GetAllUsersWithClaim :: ClaimName -> Query [User]

newtype Context m = Context
  { dataSources :: DataSources m
  }

data DataSources m = DataSources
  { tenants :: DB TenantId m Tenant,
    users :: DB UserId m User,
    claims :: DB ClaimId m Claim,
    userClaims :: DB UserId m [UserClaim]
  }

data DB i m a = DB
  { getById :: i -> m a,
    getAll :: m [a]
  }

--resolvers
type Resolver c q m a = (Monad m) => q -> ReaderT c m a

getTenantById :: Resolver (Context m) TenantId m Tenant
getTenantById tenantId = do
  tenantDb <- asks (tenants . dataSources)
  let tenant = tenantDb `getById` tenantId
  lift tenant

getUserById :: Resolver (Context m) UserId m User
getUserById userId = do
  userDB <- asks (users . dataSources)
  let user = userDB `getById` userId
  lift user

getAllUsers :: Resolver (Context m) () m [User]
getAllUsers () = do
  userDB <- asks (users . dataSources)
  let userList = getAll userDB
  lift userList

getClaimById :: Resolver (Context m) ClaimId m Claim
getClaimById claimId = do
  claimDb <- asks (claims . dataSources)
  let claim = claimDb `getById` claimId
  lift claim

getUserClaims :: Resolver (Context m) UserId m [UserClaim]
getUserClaims userId = do
  userClaimDb <- asks (userClaims . dataSources)
  let claims = userClaimDb `getById` userId
  lift claims

--todo:: Implement the following resolvers by composing existing ones
--hint: In order to keep things DRY, do not use the context at all,
--      you should only use the resolvers from above:
--      getTenantById, getUserById, getAllUsers, getClaimById, getUserClaims

getAllUsersByTenantId :: Resolver (Context m) TenantId m [User]

getTenantByUserId :: Resolver (Context m) UserId m Tenant

getUserHasClaim :: Resolver (Context m) (UserId, ClaimName) m Bool

getAllUsersWithClaim :: Resolver (Context m) ClaimName m [User]


resolver :: Resolver (Context m) (Query a) m a
resolver (GetTenantById query) = getTenantById query
resolver (GetUserById query) = getUserById query
resolver GetAllUsers = getAllUsers ()
resolver (GetClaimById query) = getClaimById query
resolver (GetAllUsersByTenantId query) = getAllUsersByTenantId query
resolver (GetTenantByUserId query) = getTenantByUserId query
resolver (GetUserHasClaim query) = getUserHasClaim query
resolver (GetAllUsersWithClaim query) = getAllUsersWithClaim query

--test
tenantMap :: Map.Map TenantId Tenant
tenantMap = Map.fromList [(1, Tenant 1 "TS")]

userMap :: Map.Map UserId User
userMap = Map.fromList [(1, User 1 "radu" 1), (2, User 2 "matei" 1)]

claimMap :: Map.Map Int Claim
claimMap = Map.fromList [(1, Claim 1 "read"), (2, Claim 2 "write")]

userClaimMap :: Map.Map UserId [UserClaim]
userClaimMap = Map.fromList [(1, [UserClaim 1 1]), (2, [UserClaim 2 1])]

fromMap :: (Ord i, Typeable a) => Map.Map i a -> DB i (MaybeT IO) a
fromMap map =
  DB
    { getById =
        \id -> do
          let v = Map.lookup id map
          liftIO $ putStrLn $ "****DEBUG: getById " ++ " " ++ show (typeOf v)
          MaybeT $ return v,
      getAll = do
        let xs = snd <$> Map.toList map
        liftIO $ putStrLn $ "****DEBUG: getAll " ++ " " ++ show (typeOf xs)
        return xs
    }

fromMap' :: (Ord i) => Map.Map i a -> DB i Maybe a
fromMap' map =
  DB
    { getById = (`Map.lookup` map),
      getAll = do
        let xs = snd <$> Map.toList map
        return xs
    }

fromMap'' :: (Ord i) => Map.Map i a -> DB i Identity a
fromMap'' map =
  DB
    { getById = return . fromJust . (`Map.lookup` map),
      getAll = do
        let xs = snd <$> Map.toList map
        return xs
    }

ctx :: Context (MaybeT IO)
ctx = Context $ DataSources (fromMap tenantMap) (fromMap userMap) (fromMap claimMap) (fromMap userClaimMap)

ctx' :: Context Maybe
ctx' = Context $ DataSources (fromMap' tenantMap) (fromMap' userMap) (fromMap' claimMap) (fromMap' userClaimMap)

ctx'' :: Context Identity
ctx'' = Context $ DataSources (fromMap'' tenantMap) (fromMap'' userMap) (fromMap'' claimMap) (fromMap'' userClaimMap)


executeQuery :: Query a -> Context (MaybeT IO) -> IO (Maybe a)
executeQuery q = runMaybeT . runReaderT (resolver q)

executeQuery' :: Query a -> Context Maybe -> Maybe a
executeQuery' = runReaderT . resolver

executeQuery'' :: Query a -> Context Identity -> a
executeQuery'' q = runIdentity . runReaderT (resolver q)

tenant1 :: IO (Maybe Tenant)
tenant1 = executeQuery (GetTenantByUserId 1) ctx

tenant1' :: Maybe Tenant
tenant1' = executeQuery' (GetTenantByUserId 1) ctx'

tenant1'' :: Tenant
tenant1'' = executeQuery'' (GetTenantByUserId 1) ctx''

user1HasReadAccess :: IO (Maybe Bool)
user1HasReadAccess = executeQuery (GetUserHasClaim (1, "read")) ctx

user1HasReadAccess' :: Maybe Bool
user1HasReadAccess' = executeQuery' (GetUserHasClaim (1, "read")) ctx'

user1HasReadAccess'' :: Bool
user1HasReadAccess'' = executeQuery'' (GetUserHasClaim (1, "read")) ctx''

allUsersWithReadAccess :: IO (Maybe [User])
allUsersWithReadAccess = executeQuery (GetAllUsersWithClaim "read") ctx

allUsersWithReadAccess' :: Maybe [User]
allUsersWithReadAccess' = executeQuery' (GetAllUsersWithClaim "read") ctx'

allUsersWithReadAccess'' :: [User]
allUsersWithReadAccess'' = executeQuery'' (GetAllUsersWithClaim "read") ctx''
