{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}

import Control.Monad
import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Maybe

--schema
data Tenant = Tenant {tenantId :: Int, tenantName :: String} deriving (Show)

data User = User {userId :: Int, userName :: String, tenantId :: Int} deriving (Show)

data Claim = Claim {claimId :: Int, claimName :: String}

data UserClaim = UserClaim {userId :: Int, claimId :: Int}

data Query a where
  GetTenantById :: Int -> Query Tenant
  GetUserById :: Int -> Query User
  GetAllUsers :: Query [User]
  GetClaimById :: Int -> Query Claim
  GetAllUsersByTenantId :: Int -> Query [User]
  GetTenantByUserId :: Int -> Query Tenant
  GetUserHasClaim :: (Int, String) -> Query Bool
  GetAllUsersWithClaim :: String -> Query [User]

newtype Context = Context
  { dataSources :: DataSources
  }

data DataSources = DataSources
  { tenants :: Map.Map Int Tenant,
    users :: Map.Map Int User,
    claims :: Map.Map Int Claim,
    userClaims :: Map.Map Int [UserClaim]
  }

--resolvers
type Resolver c q a = q -> Reader c a

getTenantById :: Resolver Context Int Tenant
getTenantById tenantId = do
  ds <- asks dataSources
  let tenant = Map.lookup tenantId (tenants ds)
  return $ fromJust tenant

getUserById :: Resolver Context Int User
getUserById userId = do
  ds <- asks dataSources
  let user = Map.lookup userId (users ds)
  return $ fromJust user

getAllUsers :: Resolver Context () [User]
getAllUsers () = do
  ds <- asks dataSources
  let userList = snd <$> Map.toList (users ds)
  return userList

getClaimById :: Resolver Context Int Claim
getClaimById claimId = do
  ds <- asks dataSources
  let claim = Map.lookup claimId (claims ds)
  return $ fromJust claim

getUserClaims :: Resolver Context Int [UserClaim]
getUserClaims userId = do
  ds <- asks dataSources
  let claims = Map.lookup userId (userClaims ds)
  return $ fromJust claims

--todo:: Implement the following resolvers by composing existing ones
--hint: You should not use the context at all
getAllUsersByTenantId :: Resolver Context Int [User]

getTenantByUserId :: Resolver Context Int Tenant

getUserHasClaim :: Resolver Context (Int, String) Bool

getAllUsersWithClaim :: Resolver Context String [User]


resolver :: Resolver Context (Query a) a
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
tenantMap :: Map.Map Int Tenant
tenantMap = Map.fromList [(1, Tenant 1 "TS")]

userMap :: Map.Map Int User
userMap = Map.fromList [(1, User 1 "radu" 1), (2, User 2 "matei" 1)]

claimMap :: Map.Map Int Claim
claimMap = Map.fromList [(1, Claim 1 "read"), (2, Claim 2 "write")]

userClaimMap :: Map.Map Int [UserClaim]
userClaimMap = Map.fromList [(1, [UserClaim 1 1]), (2, [UserClaim 2 1])]

ctx :: Context
ctx = Context $ DataSources tenantMap userMap claimMap userClaimMap

tenant1 :: Tenant
tenant1 = executeQuery (GetTenantByUserId 1) ctx

user1HasReadAccess :: Bool
user1HasReadAccess = executeQuery (GetUserHasClaim (1, "read")) ctx


allUsersWithReadAccess :: [User]
allUsersWithReadAccess = executeQuery (GetAllUsersWithClaim "read") ctx
