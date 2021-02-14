import Data.List

-- ファーストクラス関数(第一級関数)
ifEven myFunc x = if even x
  then myFunc x
  else x

inc n = n + 1
double n = n * 2
square n = n ^ 2

ifEvenInc n = ifEven inc n
ifEvenDouble n = ifEven double n
ifEvenSquare n = ifEven square n

-- カスタムソート(LastNameで並び替えたい)
names = [("Ian", "Curtis"),
  ("Bernard", "Sumner"),
  ("Peter", "Hook"),
  ("Stephen", "Morris")]

compareLastNames name1 name2 = if lastName1 > lastName2
  then GT
  else if lastName1 < lastName2
    then LT
    else EQ
  where
    lastName1 = snd name1
    lastName2 = snd name2

-- sort names
-- sortBy compareLastName names


addressLetter name location = (getLocationFunction location) name

-- dispatch(戻り値として関数を返す)
getLocationFunction location = case location of
  "ny" -> nyOffice
  "sf" -> sfOffice
  "reno" -> renoOffice
  _ -> (\name -> (fst name) ++ " " ++ (snd name))

-- ny
nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
  where nameText = (fst name) ++ " " ++ (snd name)

-- sf
sfOffice name = if lastName < "L"
  then nameText ++ " - PO Box 1234 - San Francisco, CA, 94111"
  else nameText ++ " - PO Box 1010 - San Francisco, CA, 94109"
  where
    lastName = snd name
    nameText = (fst name) ++ " " ++ lastName

-- reno
renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
  where nameText = snd name