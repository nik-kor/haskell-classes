module Main where

{--

## Higher order functions

Definition: - HOF is a function that takes other function as an input parameter

Examples(+explore the type signatures):

 - (.)
 - ($)
 - foldl

--}

-- (.) :: (b -> c) -> (a -> b) -> a -> c

f :: Bool -> String
f p = if p then "all good" else "fooo"

g :: Int -> Bool
g p = p > 0

compositionFAfterG = f . g

-- ($) :: (a -> b) -> a -> b
-- e.g. negate (x + 1)

-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b

sumElements :: Num a => [a] -> a
sumElements xs = foldl (\b a -> b + a) 0 xs

{--

## Exercise
Use `map` to add 5 to each item of a list of integers

--}

addFiveToEachItem :: Num a => [a] -> [a]
addFiveToEachItem xs = map (\a -> a + 5) xs


{--
## Pattern matching


--}

add :: (Int, Int) -> Int
add (a, b) = a + b

head' :: [a] -> a
head' (x:_) = x

-- naive implementation!
map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : (map' f xs)

{--
## ADT

"Category Theory 5.2: Algebraic data types by Bartosz Milewski" https://www.youtube.com/watch?v=w1WMykh7AxA

--}

{--
### Product data types

Tuples - https://hackage.haskell.org/package/ghc-prim-0.3.1.0/docs/src/GHC-Tuple.html
--}

myTuple = (1, 2, "hi there", True)

-- Records
data Wife = Wife {} deriving (Show)

data User = User {
      name :: String
    , age :: Int
    , married :: Bool
    , wife :: Wife
} deriving (Show)

data Customer = Customer {
    user :: User
} deriving (Show)



renderUser :: User -> String
renderUser (User n a m w) = "the name is " ++ n ++ " and the age is " ++ (show a)


{--
## Excercise

Use Pair for add
--}

data Pair = Pair { a :: Int, b :: Int }

addPair :: Pair -> Int
addPair (Pair x y) = x + y

{--
### Sum type
--}

data Bool' = True' | False'

data Maybe' a = Just' a | Nothing'

data Either' a b = Left' a | Right' b

data Status =
    Success String
    | Failure Int String
    | Pending
    | Processing (Int, Int) deriving (Show, Eq)

renderStatus :: Status -> String
renderStatus s = case s of
    Success msg         -> "success " ++ msg
    Pending             -> "pending..."
    Failure errCode msg -> show errCode ++ " " ++ msg
    Processing (x, y)   -> "processing"


{--
## Excercise

Implement safeHead

--}

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x














main :: IO ()
main = print "all good!"
