{-# LANGUAGE GADTs #-}

import Control.Monad
import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Maybe

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
  { tenants :: Map.Map TenantId Tenant,
    users :: Map.Map UserId User,
    claims :: Map.Map ClaimId Claim,
    userClaims :: Map.Map UserId [UserClaim]
  }

--resolvers
type Resolver c q a = q -> ReaderT c Maybe a

getTenantById :: Resolver Context TenantId Tenant
getTenantById tenantId = do
  ds <- asks dataSources
  let tenant = Map.lookup tenantId (tenants ds)
  lift tenant

getUserById :: Resolver Context UserId User
getUserById userId = do
  ds <- asks dataSources
  let user = Map.lookup userId (users ds)
  lift user

getAllUsers :: Resolver Context () [User]
getAllUsers () = do
  ds <- asks dataSources
  let userList = snd <$> Map.toList (users ds)
  return userList

getClaimById :: Resolver Context ClaimId Claim
getClaimById claimId = do
  ds <- asks dataSources
  let claim = Map.lookup claimId (claims ds)
  lift claim

getUserClaims :: Resolver Context UserId [UserClaim]
getUserClaims userId = do
  ds <- asks dataSources
  let claims = Map.lookup userId (userClaims ds)
  lift claims

--todo:: Implement the following resolvers by composing existing ones
--hint: In order to keep things DRY, do not use the context at all,
--      you should only use the resolvers from above: 
--      getTenantById, getUserById, getAllUsers, getClaimById, getUserClaims

getAllUsersByTenantId :: Resolver Context TenantId [User]

getTenantByUserId :: Resolver Context UserId Tenant

getUserHasClaim :: Resolver Context (UserId, ClaimName) Bool

getAllUsersWithClaim :: Resolver Context ClaimName [User]



resolver :: Resolver Context (Query a) a
resolver (GetTenantById query) = getTenantById query
resolver (GetUserById query) = getUserById query
resolver GetAllUsers = getAllUsers ()
resolver (GetClaimById query) = getClaimById query
resolver (GetAllUsersByTenantId query) = getAllUsersByTenantId query
resolver (GetTenantByUserId query) = getTenantByUserId query
resolver (GetUserHasClaim query) = getUserHasClaim query
resolver (GetAllUsersWithClaim query) = getAllUsersWithClaim query

executeQuery :: Query a -> Context -> Maybe a
executeQuery =  runReaderT . resolver

--test
tenantMap :: Map.Map TenantId Tenant
tenantMap = Map.fromList [(1, Tenant 1 "TS")]

userMap :: Map.Map UserId User
userMap = Map.fromList [(1, User 1 "radu" 1), (2, User 2 "matei" 1)]

claimMap :: Map.Map Int Claim
claimMap = Map.fromList [(1, Claim 1 "read"), (2, Claim 2 "write")]

userClaimMap :: Map.Map UserId [UserClaim]
userClaimMap = Map.fromList [(1, [UserClaim 1 1]), (2, [UserClaim 2 1])]

ctx :: Context
ctx = Context $ DataSources tenantMap userMap claimMap userClaimMap

tenant1 :: Maybe Tenant
tenant1 = executeQuery (GetTenantByUserId 1) ctx

user1HasReadAccess :: Maybe Bool
user1HasReadAccess = executeQuery (GetUserHasClaim (1, "read")) ctx


allUsersWithReadAccess :: Maybe [User]
allUsersWithReadAccess = executeQuery (GetAllUsersWithClaim "read") ctx
