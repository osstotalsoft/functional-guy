instance Semigroup Integer where
  (<>) x y = x + y

oneComposedWithTwo :: Integer
oneComposedWithTwo = 1 <> 2

showTheCompositionOf :: (Show a, Semigroup a) => a -> a -> String
showTheCompositionOf x y = show (x <> y)

