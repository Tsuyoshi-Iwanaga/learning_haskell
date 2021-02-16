-- lesson10
robot (name, attack, hp) = (\message -> message (name, attack, hp))

killerRobot = robot ("Killer3R", 15, 40)

getName aRobot = aRobot (\(n, a, h) -> n)
getAttack aRobot = aRobot (\(n, a, h) -> a)
getHP aRobot = aRobot (\(n, a, h) -> h)

setName aRobot newName = aRobot (\(n, a, h) -> robot(newName, a, h))
setAttack aRobot newAttack = aRobot (\(n, a, h) -> robot(n, newAttack, h))
setHP aRobot newHP = aRobot (\(n, a, h) -> robot(n, a, newHP))

print aRobot = aRobot (\(n, a, h) -> n ++ (show a) ++ (show h))

damage aRobot dm = aRobot (\(n, a, h) -> robot(n, a, h - dm))

fight aRobot bRobot = damage bRobot attack
  where attack = if getHP aRobot > 10 then getAttack aRobot else 0


-- robo with Type
data Robot = Robot String Int Int

killerRobot2 :: Robot
killerRobot2 = Robot "Killer4Z" 20 45

gentleRobot2 :: Robot
gentleRobot2 = Robot "JentleManFX" 5 40

showName :: Robot -> String
showName (Robot name attack hp) = name 

showAttack :: Robot -> String
showAttack (Robot name attack hp) = show attack

showHP :: Robot -> String
showHP (Robot name attack hp) = show hp

settingName :: Robot -> String -> Robot
settingName (Robot name attack hp) newName = Robot newName attack hp

settingAttack :: Robot -> Int -> Robot
settingAttack (Robot name attack hp) newAttack = Robot name newAttack hp

settingHP :: Robot -> Int -> Robot
settingHP (Robot name attack hp) newHP = Robot name attack newHP

printRobo :: Robot -> String
printRobo (Robot name attack hp) = name ++ " " ++ show attack ++ " " ++ show hp

damageRobo :: Robot -> Int -> Robot
damageRobo (Robot name attack hp) damage = Robot name attack (hp-damage)

fightRobo :: Robot -> Robot -> Robot
fightRobo robot1 robot2 = damageRobo robot2 attack
  where attack = if read (showHP robot1) > 10 then read (showAttack robot1) else 0 

-- record
data RobotRc = RobotRc {
  name :: String,
  attack :: Int,
  hp :: Int
}

robotRc = RobotRc {
  name = "killerEF5",
  attack = 250,
  hp = 300
}

-- name robotRc
-- afterRobotRc = robotRc { name = "killerXXX100" }