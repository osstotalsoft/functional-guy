instance Semigroup Integer where
  (<>) = (+)

instance Monoid Integer where
  mempty = 0


--mconcat polymorhic fn
x = mconcat [1 .. 10]

y = mconcat $ map (replicate 3) [1 .. 10]


-- how probably mconcat is written
mconcat' :: Monoid a => [a] -> a
mconcat' = foldr (<>) mempty

--more contraints means more power
fn1 :: a -> a
fn1 a = a

fn2 :: Semigroup a => a -> a
fn2 a = a <> a

fn3 :: Monoid a => a -> a
fn3 a = a <> mempty
