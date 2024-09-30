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
    (USD _) == (INR _) = False
    (INR _) == (USD _) = False

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
data BST a = Empty | Node a (BST a) (BST a)

-- this makes it so BST is part of the show type class, this is just an in order print
instance Show a => Show (BST a) where
    show Empty = ""
    show (Node x l r) = (show l ++ " " ++ show x ++ " " ++ show r)

-- this is a simple insert funciton for the BST, I think that it is correct, more testing is needed
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

searchBST :: Eq t => BST t -> t -> Bool
searchBST Empty _ = False
searchBST (Node v l r) x = if x == v
    then True
    else (searchBST l x) || (searchBST r x)

--  /$$$$$$$                       /$$           /$$$$$$$$                           
-- | $$__  $$                     | $$          | $$_____/                           
-- | $$  \ $$ /$$$$$$   /$$$$$$  /$$$$$$        | $$     /$$$$$$  /$$   /$$  /$$$$$$ 
-- | $$$$$$$/|____  $$ /$$__  $$|_  $$_/        | $$$$$ /$$__  $$| $$  | $$ /$$__  $$
-- | $$____/  /$$$$$$$| $$  \__/  | $$          | $$__/| $$  \ $$| $$  | $$| $$  \__/
-- | $$      /$$__  $$| $$        | $$ /$$      | $$   | $$  | $$| $$  | $$| $$      
-- | $$     |  $$$$$$$| $$        |  $$$$/      | $$   |  $$$$$$/|  $$$$$$/| $$      
-- |__/      \_______/|__/         \___/        |__/    \______/  \______/ |__/      


main :: IO ()
main = do
    let t = Empty
    let t' = insertBST t (USD 100)
    let t'' = insertBST t' (INR 1000)
    putStrLn (show t'')
    let res = searchBST t'' (USD 1000)
    putStrLn (show res)