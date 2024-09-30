module PA2 where

--  /$$$$$$$                       /$$            /$$$$$$                     
-- | $$__  $$                     | $$           /$$__  $$                    
-- | $$  \ $$ /$$$$$$   /$$$$$$  /$$$$$$        | $$  \ $$ /$$$$$$$   /$$$$$$ 
-- | $$$$$$$/|____  $$ /$$__  $$|_  $$_/        | $$  | $$| $$__  $$ /$$__  $$
-- | $$____/  /$$$$$$$| $$  \__/  | $$          | $$  | $$| $$  \ $$| $$$$$$$$
-- | $$      /$$__  $$| $$        | $$ /$$      | $$  | $$| $$  | $$| $$_____/
-- | $$     |  $$$$$$$| $$        |  $$$$/      |  $$$$$$/| $$  | $$|  $$$$$$$
-- |__/      \_______/|__/         \___/         \______/ |__/  |__/ \_______/

-- this is the data that we are going to be using to represent the two different types of currency
-- there are two constructors, the first being for USD and the second being for INR
data Currency = USD Double | INR Double

-- This makes it so Currency is of typeclass show
instance Show Currency where
    show (USD x) = "USD " ++ show x
    show (INR x) = "INR " ++ show x

-- this makes it so currency is also of type class Eq, we can use this for our BST algorithims that we are about to implement
instance Eq Currency where
    (USD x) == (USD y) = x == y
    (INR x) == (INR y) = x == y
    (USD _) == (INR _) = False
    (INR _) == (USD _) = False

-- this makes it so currency is also of type class Ord, which will be handy for the insertions and searches
instance Ord Currency where
    compare (USD x) (USD y) = compare x y
    compare (INR x) (INR y) = compare x y
    compare (USD x) (INR y) = compare x y
    compare (INR x) (USD y) = compare x y

-- Funciton that converts currency to its USD form
convertToUSD :: Currency -> Currency
convertToUSD (INR x) = USD (x * 0.012)
convertToUSD (USD x) = USD x

-- funciton that converts currency into its INR form
convertToINR :: Currency -> Currency
convertToINR (USD x) = INR (x * 82)
convertToINR (INR x) = INR x

-- this just swaps the currency from one thing to another, its a toggle
convertSwitch :: Currency -> Currency
convertSwitch (USD x) = INR (x * 82)
convertSwitch (INR x) = USD (x * 0.012)

