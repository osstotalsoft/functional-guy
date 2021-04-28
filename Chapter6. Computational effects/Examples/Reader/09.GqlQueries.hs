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
type Resolver q c a = q -> Reader c a

getTenantById :: Resolver TenantId Context Tenant
getTenantById tenantId = asks $ fromJust . Map.lookup tenantId . tenants . dataSources

-- getTenantById tenantId = do
--   ds <- asks dataSources
--   let tenant = Map.lookup tenantId (tenants ds)
--   return $ fromJust tenant

getUserById :: Resolver UserId Context User
getUserById userId = asks $ fromJust . Map.lookup userId . users . dataSources

-- getUserById userId = do
--   ds <- asks dataSources
--   let user = Map.lookup userId (users ds)
--   return $ fromJust user

getAllUsers :: Resolver () Context [User]
getAllUsers () = asks $ fmap snd . Map.toList . users . dataSources

-- getAllUsers () = do
--   ds <- asks dataSources
--   let userList = snd <$> Map.toList (users ds)
--   return userList

getClaimById :: Resolver ClaimId Context Claim
getClaimById claimId = asks $ fromJust . Map.lookup claimId . claims . dataSources

-- getClaimById claimId = do
--   ds <- asks dataSources
--   let claim = Map.lookup claimId (claims ds)
--   return $ fromJust claim

getUserClaims :: Resolver UserId Context [UserClaim]
getUserClaims userId = asks $ fromJust . Map.lookup userId . userClaims . dataSources

-- getUserClaims userId = do
--   ds <- asks dataSources
--   let claims = Map.lookup userId (userClaims ds)
--   return $ fromJust claims

--todo:: Implement the following resolvers by composing existing ones
--hint: In order to keep things DRY, do not use the context at all,
--      you should only use the resolvers from above: 
--      getTenantById, getUserById, getAllUsers, getClaimById, getUserClaims

getAllUsersByTenantId :: Resolver TenantId Context [User]
getAllUsersByTenantId tId = filter ((tId ==) . _tenantId) <$> getAllUsers ()

getTenantByUserId :: Resolver UserId Context Tenant
getTenantByUserId = getUserById >=> getTenantById . _tenantId

-- getTenantByUserId userId = do
--   user <- getUserById userId
--   let tid = _tenantId user
--   getTenantById tid

getUserHasClaim :: Resolver (UserId, ClaimName) Context Bool
getUserHasClaim (userId, claim) = do
  userClaims <- getUserClaims userId
  claims <- mapM getClaimById (_claimId <$> userClaims)
  return $ any (\x -> claimName x == claim) claims

getAllUsersWithClaim :: Resolver ClaimName Context [User]
getAllUsersWithClaim claim = do
  users <- getAllUsers ()
  filterM (\x -> getUserHasClaim (userId x, claim)) users


resolver :: Resolver (Query a) Context a
resolver (GetTenantById query) = getTenantById query
resolver (GetUserById query) = getUserById query
resolver GetAllUsers = getAllUsers ()
resolver (GetClaimById query) = getClaimById query
resolver (GetAllUsersByTenantId query) = getAllUsersByTenantId query
resolver (GetTenantByUserId query) = getTenantByUserId query
resolver (GetUserHasClaim query) = getUserHasClaim query
resolver (GetAllUsersWithClaim query) = getAllUsersWithClaim query

executeQuery :: Query a -> Context -> a
executeQuery = runReader . resolver

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

tenant1 :: Tenant
tenant1 = executeQuery (GetTenantByUserId 1) ctx

user1HasReadAccess :: Bool
user1HasReadAccess = executeQuery (GetUserHasClaim (1, "read")) ctx


allUsersWithReadAccess :: [User]
allUsersWithReadAccess = executeQuery (GetAllUsersWithClaim "read") ctx
