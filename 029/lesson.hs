-- Functorのfmap(<$>)をApplicativeのみで実装する
allFmap :: Applicative f => (a -> b) -> f a -> f b
allFmap fnc n = pure fnc <*> n

-- return Maybe
example :: Maybe Int
example = pure (*) <*> pure ((+) 2 4) <*> pure 6

-- calc needed Beer
startingBeer :: [Int]
startingBeer = [6, 12]

remainingBeer :: [Int]
remainingBeer = pure (-) <*> startingBeer <*> [4]

guest :: [Int]
guest = [2, 3]

totalPeople :: [Int]
totalPeople = (+ 2) <$> guest

beersPerGuest :: [Int]
beersPerGuest = [3, 4]

totalBeersNeeded :: [Int]
totalBeersNeeded = pure (*) <*> beersPerGuest <*> totalPeople

beersToPurchase :: [Int]
beersToPurchase = pure (-) <*> totalBeersNeeded <*> remainingBeer