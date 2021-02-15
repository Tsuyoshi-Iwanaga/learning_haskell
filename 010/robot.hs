-- コンストラクタ
robot (name, attack, hp) = \message -> message (name, attack, hp)

-- インスタンス生成
killerRobot = robot ("Killer3r", 15, 40)
gentleGiant = robot ("Mr. Friendly", 20, 30)

-- ゲッター
getName aRobot = aRobot (\(n, _, _) -> n)
getAttack aRobot = aRobot (\(_, a, _) -> a)
getHP aRobot = aRobot (\(_, _, h) -> h)

-- セッター
setName aRobot name = aRobot (\(n, a, h) -> robot (name, a, h))
setAttack aRobot attack = aRobot (\(n, a, h) -> robot (n, attack, h))
setHP aRobot hp = aRobot (\(n, a, h) -> robot (n, a, hp))

-- まとめて表示
printRobot aRobot = aRobot (\(n, a, h) -> n ++ "/" ++ (show a) ++ "/" ++ (show h))

-- ダメージ
damage aRobot attackDamage = aRobot (\(n, a, h) -> robot (n, a, h - attackDamage))

-- 戦う関数
fight aRobot defender = damage defender attack
  where attack = if getHP aRobot > 10 then getAttack aRobot else 0

-- 3回ずつ戦う(同時に戦うので問題が起きる)
-- gentleGiantRound1 = fight killerRobot gentleGiant
-- killerRobotRound1 = fight gentleGiant killerRobot
-- gentleGiantRound2 = fight killerRobotRound1 gentleGiantRound1
-- killerRobotRound2 = fight gentleGiantRound1 killerRobotRound1
-- gentleGiantRound3 = fight killerRobotRound2 gentleGiantRound2
-- killerRobotRound3 = fight gentleGiantRound2 killerRobotRound2

-- 3回ずつ戦う(順番を指定してあげる)
gentleGiantRound1 = fight killerRobot gentleGiant
killerRobotRound1 = fight gentleGiantRound1 killerRobot
gentleGiantRound2 = fight killerRobotRound1 gentleGiantRound1
killerRobotRound2 = fight gentleGiantRound2 killerRobotRound1
gentleGiantRound3 = fight killerRobotRound2 gentleGiantRound2
killerRobotRound3 = fight gentleGiantRound3 killerRobotRound2

-- 非同期や並列で実行される場合などに対応するためにはステートレスにする必要がある
-- 上のバージョンでは各関数がどんな順番で実行されても結果は変わらない