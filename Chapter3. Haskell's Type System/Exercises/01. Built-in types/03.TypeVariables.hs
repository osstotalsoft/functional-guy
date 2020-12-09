--polymorhic fn
identity :: a -> a
identity x = x

myChar :: Char
myChar = identity 'a'

myInteger :: Integer
myInteger = identity 1


--another polymorphic fn
makeTriple :: a -> b -> c -> (a, b, c)
makeTriple x y z = (x, y, z)

myCar = makeTriple "BMW" "X5" 3.0