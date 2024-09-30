--  /$$$$$$$                       /$$            /$$$$$$                     
-- | $$__  $$                     | $$           /$$__  $$                    
-- | $$  \ $$ /$$$$$$   /$$$$$$  /$$$$$$        | $$  \ $$ /$$$$$$$   /$$$$$$ 
-- | $$$$$$$/|____  $$ /$$__  $$|_  $$_/        | $$  | $$| $$__  $$ /$$__  $$
-- | $$____/  /$$$$$$$| $$  \__/  | $$          | $$  | $$| $$  \ $$| $$$$$$$$
-- | $$      /$$__  $$| $$        | $$ /$$      | $$  | $$| $$  | $$| $$_____/
-- | $$     |  $$$$$$$| $$        |  $$$$/      |  $$$$$$/| $$  | $$|  $$$$$$$
-- |__/      \_______/|__/         \___/         \______/ |__/  |__/ \_______/

-- the data type that we made for the currency
data Currency = USD Double | INR Double

-- providing definitions for the Show operator
instance Show Currency where
    show (USD x) = "USD " ++ show x
    show (INR x) = "INR " ++ show x

-- this is so that we can convert between INR and USD using pattern matching
convert :: Currency -> Currency
convert (USD x) = INR (x * 82)
convert (INR x) = USD (x * 0.012)

-- this is so that we can make currency of typeclass Eq, this will make it easier for tree insertions
instance Eq Currency where
    (USD x) == (USD y) = x == y
    (INR x) == (INR y) = x == y
    (USD x) == (INR y) = (x * 82) == y
    (INR x) == (USD y) = x == (y * 82)

-- this is so that we can make currency of typeclass Ord, this is going to help a lot for tree insertions
instance Ord Currency where
    compare (USD x) (USD y) = compare x y
    compare (INR x) (INR y) = compare x y
    compare (USD x) (INR y) = compare x y
    compare (INR x) (USD y) = compare x y

--  /$$$$$$$                       /$$           /$$$$$$$$                     
-- | $$__  $$                     | $$          |__  $$__/                     
-- | $$  \ $$ /$$$$$$   /$$$$$$  /$$$$$$           | $$ /$$  /$$  /$$  /$$$$$$ 
-- | $$$$$$$/|____  $$ /$$__  $$|_  $$_/           | $$| $$ | $$ | $$ /$$__  $$
-- | $$____/  /$$$$$$$| $$  \__/  | $$             | $$| $$ | $$ | $$| $$  \ $$
-- | $$      /$$__  $$| $$        | $$ /$$         | $$| $$ | $$ | $$| $$  | $$
-- | $$     |  $$$$$$$| $$        |  $$$$/         | $$|  $$$$$/$$$$/|  $$$$$$/
-- |__/      \_______/|__/         \___/           |__/ \_____/\___/  \______/ 

-- here is the definition for the BST with two constructors
data BST a = Empty | Node Currency (BST a) (BST a)

