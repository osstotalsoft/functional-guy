--Function type wiyhout type synonims
patientInfo :: String -> String -> Int -> Int -> String
patientInfo fname lname age height = name ++ " " ++ ageHeight
  where
    name = lname ++ ", " ++ fname
    ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

--type synonims
type FirstName = String

type LastName = String

type Age = Int

type Height = Int

type PatientName = (FirstName, LastName)

type PatienInfo = String

firstName :: PatientName -> FirstName
firstName = fst

lastName :: PatientName -> LastName
lastName = snd


--same function using type synonims
patientInfo' :: PatientName -> Age -> Height -> PatienInfo
patientInfo' name age height = fullName ++ " " ++ ageHeight
  where
    fullName = lastName name ++ ", " ++ firstName name
    ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"
