instance Semigroup Integer where
  (<>) x y = x + y

instance Monoid Integer where
  mempty = 0
  mappend x y = x + y

showTheCompositionOfTheList :: (Show a, Monoid a) => [a] -> String
showTheCompositionOfTheList = show . mconcat
