newtype Box a = Box a

intInaBox = Box 1

stringInABox = Box "candy"

data Pair a b = Pair a b

intAndBool :: Pair Integer Bool
intAndBool = Pair 1 True

oneAnd :: b -> Pair Integer b
oneAnd = Pair 1

oneAndGHello :: Pair Integer String
oneAndGHello = oneAnd "hello"