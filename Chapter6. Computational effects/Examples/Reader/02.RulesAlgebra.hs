import Control.Monad
import Control.Monad.Reader

type Rule a = a -> a -> a

-- implementation
data Person = Person {personId :: Int, fName :: String, lName :: String, fullName :: String, version :: Int}

rule1 :: Rule Person
rule1 model prevModel =
  if fName model /= fName prevModel
    then model {fullName = fName model ++ " " ++ lName model}
    else model

rule2 :: Rule Person
rule2 model = do
  prevModel <- ask
  let newModel = if fName model /= fName prevModel then model {version = version model + 1} else model
  return newModel

rule3 :: Rule Person
rule3 model = do
  return $ if lName model == "Popovici" then model {personId = 7} else model

composedRule :: Rule Person
composedRule = rule1 >=> rule2 >=> rule3