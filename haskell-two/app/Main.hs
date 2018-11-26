module Main where

{--

## Higher order functions

Definition: - HOF is a function that takes other function as an input parameter

Examples(+explore the type signatures):

 - (.)
 - ($)
 - foldr

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
sumElements = undefined

{--

## Exercise
Use `map` to add 5 to each item of a list of integers

--}

addFiveToEachItem :: Num a => [a] -> [a]
addFiveToEachItem xs = undefined


{--
## Pattern matching


--}

add :: (Int, Int) -> Int
add = undefined

head' :: [a] -> a
head' = undefined

map' :: (a -> b) -> [a] -> [b]
map' = undefined


{--
## ADT

Great explanation of Product/Sum by Philip Wadler https://www.youtube.com/watch?v=V10hzjgoklA

--}

{--
### Product data types

Tuples - https://hackage.haskell.org/package/ghc-prim-0.3.1.0/docs/src/GHC-Tuple.html
--}

myTuple = (1, 2, "hi there", True)

-- Records

data User = User {
      name :: String
    , age :: Int
    , married :: Bool
} deriving (Show)

renderUser :: User -> String
renderUser = undefined


{--
## Excercise

Use Pair for add
--}

data Pair = Pair { a :: Int, b :: Int }

addPair :: Pair -> Int
addPair = undefined

{--
### Sum type
--}

data Bool' = True' | False'

data Maybe' a = Just' a | Nothing

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
safeHead = undefined













main :: IO ()
main = print "all good!"
