data Bool' = True' | False' deriving (Show)

data Sex = Male | Female

--pattern matching on data constructors
greet :: Sex -> String
greet Male = "Sir"
greet Female = "M'lady"

type CardNumber = String

type CVC = String

type CardInfo = (CardNumber, CVC)

data PaymentMethod = CreditCard CardInfo | Cash

myPaymentMethod1 = CreditCard ("my-card-number", "cvc")

myPaymentMethod2 = Cash

--pattern matching on data constructors
acceptPayment :: Double -> PaymentMethod -> String
acceptPayment amount (CreditCard (cardNumber, cvc)) = show (amount, cardNumber, cvc)
acceptPayment amount Cash = show (amount, "cash")
