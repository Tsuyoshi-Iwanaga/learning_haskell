-- 型シノニム(別名をつける)
type FirstName = String
type LastName = String
type MiddleName = String
type Age = Int
type Height = Int

-- Name型
data Name = Name FirstName LastName | NameWithMiddleName FirstName MiddleName LastName

showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddleName f m l) = f ++ " " ++ m ++ " " ++l

patientInfo :: Name -> Age -> Height -> String
patientInfo name age height = (showName name) ++ "/" ++ (show age) ++ "/" ++ (show height)

-- 血液型
data RhType = Pos | Neg
data ABOType = A | B | O | AB
data BloodType = BloodType ABOType RhType

-- インスタンスの作成
patient1BT :: BloodType
patient1BT = BloodType A Pos

patient2BT :: BloodType
patient2BT = BloodType O Neg

patient3BT :: BloodType
patient3BT = BloodType AB Pos

-- メッセージ
showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO O = "O"
showABO AB = "AB"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo (BloodType AB _) _ = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False

donarFor :: Patient -> Patient -> Bool
donarFor p1 p2 = canDonateTo (bloodType p1) (bloodType p2)

-- Sex
data Sex = Male | Female

showSex :: Sex -> String
showSex Male = "Male"
showSex Female = "Female"

-- Patient
-- data Patient = Patient Name Sex Int Int Int BloodType
data Patient = Patient {
  name :: Name,
  sex :: Sex,
  age :: Int,
  height :: Int,
  weight :: Int,
  bloodType :: BloodType
}

-- instance
johnDoe :: Patient
-- johnDoe = Patient (Name "John" "Doe") Male 30 74 200 (BloodType AB Pos)
johnDoe = Patient {
  name = Name "john" "Doe",
  age = 43,
  sex = Female,
  height = 62,
  weight = 115,
  bloodType = BloodType O Neg
}

janeElizabethSmith :: Patient
-- janeElizabethSmith = Patient (NameWithMiddleName "Jane" "Elizabeth" "Smith") Female 45 82 300 (BloodType A Neg)
janeElizabethSmith = Patient {
  name = NameWithMiddleName "Jane" "Elizabeth" "Smith",
  age = 45,
  sex = Male,
  height = 82,
  weight = 300,
  bloodType = BloodType A Neg
}

-- レコード構文で記載すればゲッターが自動的に使えるようになる
-- height johnDoe

-- レコード構文のフィールドに対して値を設定することも可能
-- johnDoeUpdate = johnDoe { age = 44 }

patientSummary :: Patient -> String
patientSummary patient = "**************\n" ++
  "Sex: " ++ showSex (sex patient) ++ "\n" ++
  "Age: " ++ show (age patient) ++ "\n" ++
  "Height: " ++ show (height patient) ++ "\n" ++
  "Weight: " ++ show (weight patient) ++ "\n" ++
  "Blood Type: " ++ showBloodType (bloodType patient) ++ "\n" ++
  "****************\n"