{-# LANGUAGE FlexibleInstances #-}

import Control.Monad
import Control.Monad.Reader

data Lens a b = Lens
  { get :: a -> b,
    set :: a -> b -> a
  }

newtype Rule a b = Rule {runRule :: a -> a -> b} -- model -> Reader model model

type Predicate a = a -> a -> Bool

instance Semigroup (Rule a a) where
  r1 <> r2 = Rule (runRule r1 >=> runRule r2)

instance Monoid (Rule a a) where
  mempty = Rule return

ruleFor :: Lens a b -> Rule a b -> Rule a a
ruleFor l r =
  Rule
    ( \model prevModel ->
        let newB = runRule r model prevModel
         in set l model newB
    )

(==>) :: Lens a b -> Rule a b -> Rule a a
(==>) = ruleFor

updateWith :: Rule a a -> Lens a b -> b -> a -> a
updateWith r l v m = runRule r m' m
  where
    m' = set l m v

when' :: Predicate a -> Rule a a -> Rule a a
when' predicate rule =
  Rule
    ( \a -> do
        cond <- predicate a
        if cond then runRule rule a else return a
    )

(|>) :: Rule a a -> Predicate a -> Rule a a
rule |> predicate = when' predicate rule

prop :: (a -> b) -> Predicate b -> Predicate a
prop selector pred a = do
  a' <- ask
  let b = selector a
  let b' = selector a'
  let cond = pred b b'
  return cond

-- implementation
data Address = Address {city :: String, number :: Int} deriving (Show)

data Person = Person {personId :: Int, fName :: String, lName :: String, fullName :: String, address :: Maybe Address, version :: Int} deriving (Show)

-- autogenerated stuff
personId' :: Lens Person Int
personId' = Lens personId (\person personId -> person {personId = personId})

fName' :: Lens Person String
fName' = Lens fName (\person fName -> person {fName = fName})

lName' :: Lens Person String
lName' = Lens lName (\person lName -> person {lName = lName})

fullName' :: Lens Person String
fullName' = Lens fullName (\person fullName -> person {fullName = fullName})

version' :: Lens Person Int
version' = Lens version (\person version -> person {version = version})

address' :: Lens Person (Maybe Address)
address' = Lens address (\person address -> person {address = address})

city' :: Lens Address String
city' = Lens city (\address city -> address {city = city})

number' :: Lens Address Int
number' = Lens number (\address number -> address {number = number})

rules :: Rule Person Person
rules =
  mconcat
    [ fullName' ==> Rule (\person -> return $ fName person ++ " " ++ lName person),
      personId' ==> Rule (\person -> return 7) |> prop lName (return . (== "Popovici")),
      version'  ==> Rule (return . (+ 1) . version) |> prop fullName (/=),
      address'  ==> Rule (return . const Nothing ) |> prop personId (/=)
      -- address'  |=> mconcat
      --     [ city' ==> Rule (return . city),
      --       number' ==> Rule (return . number)
      --     ]
    ]

verbosePersonIdRule :: Rule Person Person
verbosePersonIdRule =
  personId' ==> Rule (\person -> return $ if lName person == "Popovici" then 7 else personId person)

verboseVersionRule :: Rule Person Person
verboseVersionRule =
  version'
    ==> Rule
      ( \person -> do
          prevPerson <- ask
          return $ if fullName person /= fullName prevPerson then version person + 1 else version person
      )

addressRules :: Rule Address Address
addressRules =
  mconcat
    [ city' ==> Rule (return . city),
      number' ==> Rule (return . number)
    ]

person :: Person
person = Person 1 "Radu" "Popovici" " " Nothing 0

updated :: Person
updated = updateWith rules fName' "Matei" person
