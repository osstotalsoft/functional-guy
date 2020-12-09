data CreditCardInfo = CreditCardInfo String String

show' :: CreditCardInfo -> String
show' (CreditCardInfo cardNumber cvc) = cardNumber ++ " " ++ cvc


--mixing with type synonims
data Cube' = Cube' Int Int Int


type Width = Int

type Height = Int

type Depth = Int

data Cube = Cube Width Height Depth

show'' :: Cube -> String
show'' (Cube w h d) = show w ++ " " ++ show h ++ " " ++ show d