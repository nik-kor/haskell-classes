module Main where

import Data.List
import Data.Function (on)
{--
## Haskell is like LEGO

An example of sorting lists

--}

data Person = Person { name :: String, age :: Int }
    deriving (Show, Eq, Ord)

p1, p2, p3, p4 :: Person

p1 = Person { name = "Bob", age = 21 }
p2 = Person "Alice" 22
p3 = Person "John" 77
p4 = Person "Alice" 22

ps :: [Person]
ps = [p1, p2, p3, p4]

{--
    Lets try to use `sort`.
--}

{--
    ...but want if we want to sort by age.
    Hint: use `sortBy` and `compare`
--}

{--
    Lets make it more declarative.
    Hint: use `on`
--}


{--
## Recursion

Recursion is defining a function in terms of itself via self-referential
expressions.
--}

-- 1 1 2 3 5 8 13 21 ...
fib :: Int -> Int
fib = undefined

{--
### excercise

4! = 4 * 3 * 2 * 1

--}
fact :: Int -> Int
fact = undefined

{--
And sometimes we need to keep interim value
--}

divideBy :: Int -> Int -> (Int, Int)
divideBy = undefined
{--
### excercise

--}

getMaxNumber :: [Int] -> Int
getMaxNumber = undefined

{--
## Recursive data structures
--}


-- data [] a = [] | a : [a]
data List a = TBD

-- map :: (a -> b) -> [a] -> [b]

mapList :: (a -> b) -> List a -> List b
mapList = undefined

{--
### excercise

--}

listLength :: List a -> Int
listLength = undefined

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show)

{--
Lets do the tree mapping
--}

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree = undefined

{--
An example of using recursive DS
https://youtu.be/xAZLceCZGks?t=1158 "Live coding a Mondrian generator" by Ju Liu
--}

{--
https://en.wikibooks.org/wiki/Haskell/Recursion#Don't_get_TOO_excited_about_recursion...
--}











main :: IO ()
main = print "all good!"