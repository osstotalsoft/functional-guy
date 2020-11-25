patientInfo :: String -> String -> Int -> Int -> String
patientInfo fname lname age height = name ++ " " ++ ageHeight
  where
    name = lname ++ ", " ++ fname
    ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

type FirstName = String

type LastName = String

type Age = Int

type Height = Int

type PatientName = (String, String)

firstName :: PatientName -> String
firstName = fst

lastName :: PatientName -> String
lastName = snd

patientInfo' :: PatientName -> Age -> Height -> String
patientInfo' name age height = fullName ++ " " ++ ageHeight
  where
    fullName = lastName name ++ ", " ++ firstName name
    ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"
