{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

import Control.Monad
import Control.Monad.Reader

data Lens a b = Lens
  { get :: a -> b,
    set :: a -> b -> a
  }

newtype Rule a b = Rule {runRule :: a -> a -> b} -- model -> Reader model model

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

with :: Rule a a -> Lens a b -> b -> a -> a
with r l v m = runRule r m' m
  where
    m' = set l m v

-- implementation
data Person = Person {personId :: Int, fName :: String, lName :: String, fullName :: String, version :: Int}

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

rules :: Rule Person Person
rules =
  mconcat
    [ fullName' ==> Rule (\person -> return $ fName person ++ " " ++ lName person),
      personId' ==> Rule (\person -> return $ if lName person == "Popovici" then 7 else personId person),
      version'
        ==> Rule
          ( \person -> do
              prevPerson <- ask
              return $ if fullName person /= fullName prevPerson then version person + 1 else version person
          )
    ]

person :: Person
person = Person 1 "Radu" "Popovici" "" 0

updated :: Person
updated = with rules fName' "Matei" person
