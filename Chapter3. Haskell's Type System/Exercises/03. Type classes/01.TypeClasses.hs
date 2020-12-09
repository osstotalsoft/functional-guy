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