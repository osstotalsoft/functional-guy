{-# LANGUAGE GADTs #-}

import Control.Monad
import Control.Monad.Reader

data Lens a b = Lens
  { get :: a -> b,
    set :: a -> b -> a
  }

newtype Rule a = Rule {runRule :: a -> a -> a}

instance Semigroup (Rule a) where
  r1 <> r2 = Rule (runRule r1 >=> runRule r2)

instance Monoid (Rule a) where
  mempty = Rule return


ruleFor :: Lens a b -> (a -> a -> b) -> Rule a
ruleFor l f =
  Rule
    ( \model prevModel ->
        let newB = f model prevModel
         in set l model newB
    )

(==>) :: Lens a b -> (a -> a -> b) -> Rule a
(==>) = ruleFor

with :: Rule a -> Lens a b -> b -> a -> a
with r l v m = runRule r m' m
  where m' = set l m v

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

rule2 :: Rule Person
rule2 = ruleFor fullName' (\person -> return $ fName person ++ " " ++ lName person)

rule3 :: Rule Person
rule3 = ruleFor personId' (\person -> return $ if lName person == "Popovici" then 7 else personId person)

rules :: Rule Person
rules =
  mconcat
    [ fullName' ==> (\person -> return $ fName person ++ " " ++ lName person),
      personId' ==> (\person -> return $ if lName person == "Popovici" then 7 else personId person)
    ]

person :: Person
person = Person 1 "Radu" "Popovici" "" 0

updated :: Person
updated = with rules fName' "Matei" person
