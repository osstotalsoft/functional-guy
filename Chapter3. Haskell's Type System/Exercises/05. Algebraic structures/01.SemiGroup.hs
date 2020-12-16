{-# LANGUAGE GADTs #-}

--instance Semigroup Integer - Not very helpful!
instance Semigroup Integer where
  (<>) x y = x + y

oneComposedWithTwo :: Integer
oneComposedWithTwo = 1 <> 2

-- ValidationResult Semigroup
data ValidationResult = ValidationResult {isValid :: Bool, errors :: [String]} deriving (Show)

merge :: ValidationResult -> ValidationResult -> ValidationResult
merge x y = ValidationResult {isValid = isValid x && isValid y, errors = errors x <> errors y}

instance Semigroup ValidationResult where
  (<>) = merge

-- Min a Semigroup
newtype Min a = Min {value :: a} deriving (Show, Eq, Ord)

instance Ord a => Semigroup (Min a) where
  (<>) = min

-- constraint polymorphic fn
composeWithSelf :: (Semigroup a) => a -> a
composeWithSelf x = x <> x

x = composeWithSelf 1

y = composeWithSelf ValidationResult {isValid = False, errors = ["wrong"]}

z = composeWithSelf (Min 3)

-- a more generalized version of ValidationResult using a parameterized type
data ValidationResult' a = ValidationResult' {isValid' :: Bool, errors' :: a} deriving (Show)

merge' :: Semigroup a => ValidationResult' a -> ValidationResult' a -> ValidationResult' a
merge' x y = ValidationResult' {isValid' = isValid' x && isValid' y, errors' = errors' x <> errors' y}

instance Semigroup a => Semigroup (ValidationResult' a) where
  (<>) = merge'

v = composeWithSelf ValidationResult' {isValid' = False, errors' = ["wrong"]}

v' = composeWithSelf ValidationResult' {isValid' = False, errors' = "wrong."}

v'' = composeWithSelf ValidationResult' {isValid' = False, errors' = Min 5.0}

v''' = composeWithSelf ValidationResult' {isValid' = False, errors' = [1]}

--parameterized type constraints using GADTS (on data constructors not on type constructors)
data T a where
  T :: Semigroup a => a -> T a

instance Semigroup (T a) where
  (T x) <> (T y) = T (x <> y)

myFunc :: Semigroup a => T a -> T a
myFunc (T a) = T (a <> a)

myFunc2 :: Semigroup a => a -> T a
myFunc2 = T

myAbsurdFn :: T Double -> ()
myAbsurdFn (T x) = ()

myOkFn :: T a -> T a
myOkFn t = t <> t
