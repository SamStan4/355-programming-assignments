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
    compare (USD x) (INR y) = compare (x * 82) y
    compare (INR x) (USD y) = compare x (y * 82)

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
        

--  /$$$$$$$                       /$$           /$$$$$$$$                     
-- | $$__  $$                     | $$          |__  $$__/                     
-- | $$  \ $$ /$$$$$$   /$$$$$$  /$$$$$$           | $$ /$$  /$$  /$$  /$$$$$$ 
-- | $$$$$$$/|____  $$ /$$__  $$|_  $$_/           | $$| $$ | $$ | $$ /$$__  $$
-- | $$____/  /$$$$$$$| $$  \__/  | $$             | $$| $$ | $$ | $$| $$  \ $$
-- | $$      /$$__  $$| $$        | $$ /$$         | $$| $$ | $$ | $$| $$  | $$
-- | $$     |  $$$$$$$| $$        |  $$$$/         | $$|  $$$$$/$$$$/|  $$$$$$/
-- |__/      \_______/|__/         \___/           |__/ \_____/\___/  \______/ 

-- here is the definition for the BST data structure that we are going to be working with
data BST a = Empty | Node a (BST a) (BST a)

-- simple in order print for the tree
inOrderPrint :: Show a => BST a -> [Char]
inOrderPrint n = case n of
    Empty -> ""
    (Node v l r) -> (inOrderPrint l) ++ " " ++ (show v) ++ " " ++ (inOrderPrint r)

-- simple reverse order print of the tree
reverseOrderPrint :: Show a => BST a -> [Char]
reverseOrderPrint n = case n of
    Empty -> ""
    (Node v l r) -> (reverseOrderPrint r) ++ " " ++ (show v) ++ " " ++ (reverseOrderPrint l)

-- this print gives a better look into the actual structure of the tree
structuredPrint :: Show a => BST a -> [Char]
structuredPrint n = case n of
    Empty -> "Empty"
    (Node v l r) -> "<(" ++ (show v) ++ "), (" ++ (structuredPrint l) ++ "), (" ++ (structuredPrint r) ++ ")>"

-- this makes the BST an instance of the show type class
-- the show operation defined is an in order print
instance Show a => Show (BST a) where
    show t = inOrderPrint t

-- super simple insert function for a BST
insertBST :: Ord t => BST t -> t -> BST t
insertBST Empty x = Node x Empty Empty
insertBST (Node v l r) x = if x > v 
    then Node v l (insertBST r x)
    else Node v (insertBST l x) r

--  /$$$$$$$                       /$$           /$$$$$$$$ /$$                                    
-- | $$__  $$                     | $$          |__  $$__/| $$                                    
-- | $$  \ $$ /$$$$$$   /$$$$$$  /$$$$$$           | $$   | $$$$$$$   /$$$$$$   /$$$$$$   /$$$$$$ 
-- | $$$$$$$/|____  $$ /$$__  $$|_  $$_/           | $$   | $$__  $$ /$$__  $$ /$$__  $$ /$$__  $$
-- | $$____/  /$$$$$$$| $$  \__/  | $$             | $$   | $$  \ $$| $$  \__/| $$$$$$$$| $$$$$$$$
-- | $$      /$$__  $$| $$        | $$ /$$         | $$   | $$  | $$| $$      | $$_____/| $$_____/
-- | $$     |  $$$$$$$| $$        |  $$$$/         | $$   | $$  | $$| $$      |  $$$$$$$|  $$$$$$$
-- |__/      \_______/|__/         \___/           |__/   |__/  |__/|__/       \_______/ \_______/

-- this is just the haskell implementation of the classic BST search method
searchBST :: Ord t => BST t -> t -> Bool
searchBST Empty _ = False
searchBST (Node v l r) x =
    if x == v then True
    else if x < v then searchBST l x   
    else searchBST r x

--  /$$$$$$$                       /$$           /$$$$$$$$                           
-- | $$__  $$                     | $$          | $$_____/                           
-- | $$  \ $$ /$$$$$$   /$$$$$$  /$$$$$$        | $$     /$$$$$$  /$$   /$$  /$$$$$$ 
-- | $$$$$$$/|____  $$ /$$__  $$|_  $$_/        | $$$$$ /$$__  $$| $$  | $$ /$$__  $$
-- | $$____/  /$$$$$$$| $$  \__/  | $$          | $$__/| $$  \ $$| $$  | $$| $$  \__/
-- | $$      /$$__  $$| $$        | $$ /$$      | $$   | $$  | $$| $$  | $$| $$      
-- | $$     |  $$$$$$$| $$        |  $$$$/      | $$   |  $$$$$$/|  $$$$$$/| $$      
-- |__/      \_______/|__/         \___/        |__/    \______/  \______/ |__/      

-- this is the function that takes care of summing all values in a BST of currency, then it returns the sum in USD
sumAllUSDHelper :: BST Currency -> Double
sumAllUSDHelper n = case n of
    Empty -> 0
    (Node (USD v) l r) -> v + (sumAllUSDHelper l) + (sumAllUSDHelper r)
    (Node (INR v) l r) -> (v * 0.012) + (sumAllUSDHelper l) + (sumAllUSDHelper r)
sumAllUSD :: BST Currency -> Currency
sumAllUSD t = USD $ sumAllUSDHelper t

-- this is the function that takes care of summing all values in a BST of currency, then it returns the sum in INR
sumAllINRHelper :: BST Currency -> Double
sumAllINRHelper n = case n of
    Empty -> 0
    (Node (USD v) l r) -> (v * 82) + (sumAllINRHelper l) + (sumAllINRHelper r)
    (Node (INR v) l r) -> v + (sumAllINRHelper l) + (sumAllINRHelper r)
sumAllINR :: BST Currency -> Currency
sumAllINR t = INR $ sumAllINRHelper t

--  /$$$$$$$                       /$$           /$$$$$$$$ /$$                    
-- | $$__  $$                     | $$          | $$_____/|__/                    
-- | $$  \ $$ /$$$$$$   /$$$$$$  /$$$$$$        | $$       /$$ /$$    /$$ /$$$$$$ 
-- | $$$$$$$/|____  $$ /$$__  $$|_  $$_/        | $$$$$   | $$|  $$  /$$//$$__  $$
-- | $$____/  /$$$$$$$| $$  \__/  | $$          | $$__/   | $$ \  $$/$$/| $$$$$$$$
-- | $$      /$$__  $$| $$        | $$ /$$      | $$      | $$  \  $$$/ | $$_____/
-- | $$     |  $$$$$$$| $$        |  $$$$/      | $$      | $$   \  $/  |  $$$$$$$
-- |__/      \_______/|__/         \___/        |__/      |__/    \_/    \_______/

-- a very simple function that converts all values in the BST to USD currency
convertAllUSD :: BST Currency -> BST Currency
convertAllUSD n = case n of
    Empty -> Empty
    (Node v l r) -> Node (convertToUSD v) (convertAllUSD l) (convertAllUSD r)

-- a very simple function that converts all values in the BST to INR currency
convertAllINR :: BST Currency -> BST Currency
convertAllINR n = case n of
    Empty -> Empty
    (Node v l r) -> Node (convertToINR v) (convertAllINR l) (convertAllINR r)

--  /$$$$$$$                       /$$            /$$$$$$  /$$          
-- | $$__  $$                     | $$           /$$__  $$|__/          
-- | $$  \ $$ /$$$$$$   /$$$$$$  /$$$$$$        | $$  \__/ /$$ /$$   /$$
-- | $$$$$$$/|____  $$ /$$__  $$|_  $$_/        |  $$$$$$ | $$|  $$ /$$/
-- | $$____/  /$$$$$$$| $$  \__/  | $$           \____  $$| $$ \  $$$$/ 
-- | $$      /$$__  $$| $$        | $$ /$$       /$$  \ $$| $$  >$$  $$ 
-- | $$     |  $$$$$$$| $$        |  $$$$/      |  $$$$$$/| $$ /$$/\  $$
-- |__/      \_______/|__/         \___/         \______/ |__/|__/  \__/

-- this instance of fmap on the BST makes it so that the BST data type is of type class functor, super lame, thought this would be harder
instance Functor BST where
    fmap _ Empty = Empty
    fmap f (Node v l r) = Node (f v) (fmap f l) (fmap f r)

convertAllUSDfmap :: Functor f => f Currency -> f Currency
convertAllUSDfmap t = convertToUSD `fmap` t

convertAllINRfmap :: Functor f => f Currency -> f Currency
convertAllINRfmap t = convertToUSD `fmap` t