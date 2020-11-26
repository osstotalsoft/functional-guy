patientInfo :: String -> String -> Int -> Int -> String
patientInfo fname lname age height = name ++ " " ++ ageHeight
  where
    name = lname ++ ", " ++ fname
    ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

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

patientInfo' :: PatientName -> Age -> Height -> PatienInfo
patientInfo' name age height = fullName ++ " " ++ ageHeight
  where
    fullName = lastName name ++ ", " ++ firstName name
    ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"
