{-# LANGUAGE GADTs #-}

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
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

newtype Context = Context
  { dataSources :: DataSources
  }

data DataSources = DataSources
  { tenants :: DB TenantId Tenant,
    users :: DB UserId User,
    claims :: DB ClaimId Claim,
    userClaims :: DB UserId [UserClaim]
  }

data DB i a = DB
  { getById :: i -> MaybeT IO a,
    getAll :: MaybeT IO [a]
  }

--resolvers
type Resolver q c a = q -> ReaderT c (MaybeT IO) a

getTenantById :: Resolver TenantId Context Tenant
getTenantById tenantId = do
  liftIO $ putStrLn $ "****DEBUG: getTenantById " ++ " " ++ show tenantId
  tenantDb <- asks (tenants . dataSources)
  let tenant = tenantDb `getById` tenantId
  lift tenant

getUserById :: Resolver UserId Context User
getUserById userId = do
  userDB <- asks (users . dataSources)
  let user = userDB `getById` userId
  lift user

getAllUsers :: Resolver () Context [User]
getAllUsers () = do
  userDB <- asks (users . dataSources)
  let userList = getAll userDB
  lift userList

getClaimById :: Resolver ClaimId Context Claim
getClaimById claimId = do
  claimDb <- asks (claims . dataSources)
  let claim = claimDb `getById` claimId
  lift claim

getUserClaims :: Resolver UserId Context [UserClaim]
getUserClaims userId = do
  userClaimDb <- asks (userClaims . dataSources)
  let claims = userClaimDb `getById` userId
  lift claims

--todo:: Implement the following resolvers by composing existing ones
--hint: In order to keep things DRY, do not use the context at all,
--      you should only use the resolvers from above:
--      getTenantById, getUserById, getAllUsers, getClaimById, getUserClaims

getAllUsersByTenantId :: Resolver TenantId Context [User]

getTenantByUserId :: Resolver UserId Context Tenant

getUserHasClaim :: Resolver (UserId, ClaimName) Context Bool

getAllUsersWithClaim :: Resolver ClaimName Context [User]


resolver :: Resolver (Query a) Context a
resolver (GetTenantById query) = getTenantById query
resolver (GetUserById query) = getUserById query
resolver GetAllUsers = getAllUsers ()
resolver (GetClaimById query) = getClaimById query
resolver (GetAllUsersByTenantId query) = getAllUsersByTenantId query
resolver (GetTenantByUserId query) = getTenantByUserId query
resolver (GetUserHasClaim query) = getUserHasClaim query
resolver (GetAllUsersWithClaim query) = getAllUsersWithClaim query

executeQuery :: Query a -> Context -> IO (Maybe a)
executeQuery q c = runMaybeT (runReaderT (resolver q) c)

--test
tenantMap :: Map.Map TenantId Tenant
tenantMap = Map.fromList [(1, Tenant 1 "TS")]

userMap :: Map.Map UserId User
userMap = Map.fromList [(1, User 1 "radu" 1), (2, User 2 "matei" 1)]

claimMap :: Map.Map Int Claim
claimMap = Map.fromList [(1, Claim 1 "read"), (2, Claim 2 "write")]

userClaimMap :: Map.Map UserId [UserClaim]
userClaimMap = Map.fromList [(1, [UserClaim 1 1]), (2, [UserClaim 2 1])]

fromMap :: (Ord i, Typeable a) => Map.Map i a -> DB i a
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

ctx :: Context
ctx = Context $ DataSources (fromMap tenantMap) (fromMap userMap) (fromMap claimMap) (fromMap userClaimMap)

tenant1 :: IO (Maybe Tenant)
tenant1 = executeQuery (GetTenantByUserId 1) ctx

user1HasReadAccess :: IO (Maybe Bool)
user1HasReadAccess = executeQuery (GetUserHasClaim (1, "read")) ctx

allUsersWithReadAccess :: IO (Maybe [User])
allUsersWithReadAccess = executeQuery (GetAllUsersWithClaim "read") ctx
