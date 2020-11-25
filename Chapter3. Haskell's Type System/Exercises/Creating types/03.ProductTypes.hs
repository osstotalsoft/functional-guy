type CardNumber = String

type CVC = String

data CreditCardInfo = CreditCardInfo CardNumber CVC

show' :: CreditCardInfo -> String
show' (CreditCardInfo cardNumber cvc) = cardNumber ++ " " ++ cvc

type Width = Int

type Height = Int

type Depth = Int

data Cube = Cube Width Height Depth