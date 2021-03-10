import Control.Monad

newtype Reader e a = Reader {runReader :: e -> a}

instance Functor (Reader e) where
  fmap f ra = Reader (f . runReader ra)

instance Applicative (Reader e) where
  pure = Reader . const
  rf <*> ra =
    Reader
      ( \e ->
          let f = runReader rf e
              a = runReader ra e
           in f a
      )

instance Monad (Reader e) where
  ra >>= k =
    Reader
      ( \e ->
          let a = runReader ra e
              rb = k a
           in runReader rb e
      )

ask :: Reader a a
ask = Reader Prelude.id

type Rule a = a -> Reader a a

createRule :: (t -> e -> a) -> t -> Reader e a
createRule fn model = Reader (fn model)

-- implementation
data Person = Person {id :: Int, fName :: String, lName :: String, fullName :: String, version :: Int}

rule1 :: Rule Person
rule1 = createRule (\model prevModel -> if fName model /= fName prevModel then model {version = version model + 1} else model)

rule2 :: Rule Person
rule2 = createRule (\model prevModel -> if fName model /= fName prevModel then model {fullName = fName model ++ " " ++ lName model} else model)

composedRule :: Rule Person
composedRule = rule1 >=> rule2