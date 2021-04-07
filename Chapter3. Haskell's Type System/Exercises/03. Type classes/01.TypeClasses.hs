class Printable a where
  toString :: a -> String

data Icecream = Chocolate | Vanilla deriving (Show)

instance Printable Icecream where
  toString Chocolate = "Chocolate"
  toString Vanilla = "Vanilla"

choco = toString Chocolate

choco' = show Chocolate

pretyPrint :: Printable a => a -> String
pretyPrint x = "[" ++ toString x ++ "]"


class Mappable f where
  map' :: (a -> b) -> f a -> f b

instance Mappable Maybe where
  map' f (Just a) = Just (f a)
  map' f Nothing = Nothing

instance Mappable [] where
  map' f [] = []
  map' f (x:xs) = f x : map' f xs

instance Mappable ((->)a) where
  map' f v = f . v 
