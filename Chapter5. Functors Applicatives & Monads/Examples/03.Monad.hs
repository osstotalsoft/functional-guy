import Control.Monad
import qualified Data.Map as Map

parse :: String -> Int
parse = read

isEven :: Int -> Bool
isEven = even

-- in Haskell we create new functions by composing existing ones
isEvenStr :: String -> Bool
isEvenStr = isEven . parse

compose :: (a -> b) -> (b -> c) -> a -> c
compose = flip (.)

--compose' :: (a -> m b) -> (b -> m c) -> a -> m c
compose' :: (Functor m) => (a -> m b) -> (b -> m c) -> a -> m (m c)
compose' f g = fmap g . f

flatten :: (Monad m) => m (m a) -> m a
flatten x = x >>= id
--flatten = join

compose'' :: (Monad m) => (a -> m b) -> (b -> m c) -> a -> m c
compose'' f g = flatten . fmap g . f

compose''' :: (Monad m) => (a -> m b) -> (b -> m c) -> a -> m c
compose''' f g = (\a -> f a >>= g)
--compose''' f g = (>>= g) . f
--compose''' = (>=>)

type UserName = String

type GamerId = Int

type PlayerCredits = Int

userNameDB :: Map.Map GamerId UserName
userNameDB =
  Map.fromList
    [ (1, "nYarlathoTep"),
      (2, "KINGinYELLOW"),
      (3, "dagon1997"),
      (4, "rcarter1919"),
      (5, "xCTHULHUx"),
      (6, "yogSOThoth")
    ]

creditsDB :: Map.Map UserName PlayerCredits
creditsDB =
  Map.fromList
    [ ("nYarlathoTep", 2000),
      ("KINGinYELLOW", 15000),
      ("dagon1997", 300),
      ("rcarter1919", 12),
      ("xCTHULHUx", 50000),
      ("yogSOThoth", 150000)
    ]

lookupUserName :: GamerId -> Maybe UserName
lookupUserName id = Map.lookup id userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits username = Map.lookup username creditsDB

creditsFromId :: GamerId -> Maybe PlayerCredits
creditsFromId = lookupUserName >=> lookupCredits 