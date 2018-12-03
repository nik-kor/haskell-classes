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
fib 0 = 1
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

{--
### excercise

4! = 4 * 3 * 2 * 1

--}
fact :: Int -> Int
fact 1 = 1
fact n = n * fact (n - 1)

{--
And sometimes we need to keep interim value
--}

divideBy :: Int -> Int -> (Int, Int)
divideBy n den = go n den 0
        where go n d count
                | n < d = (count, n)
                | otherwise = go (n - d) d (count + 1)
{--
### excercise

--}

getMaxNumber :: [Int] -> Int
getMaxNumber (x:xs) = go xs x
        where
            go [] m = m
            go (x:xs) m = go xs (if x > m then x else m)

{--
## Recursive data structures
--}


-- data [] a = [] | a : [a]
data List a = Nil | Cons a (List a) deriving (Show)

-- map :: (a -> b) -> [a] -> [b]

mapList :: (a -> b) -> List a -> List b
mapList _ Nil = Nil
mapList f (Cons a l) = Cons (f a) (mapList f l)

{--
### excercise

--}
listLength :: List a -> Int
listLength Nil = 0
listLength (Cons a l) = 1 + (listLength l)

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show)

{--
Lets do the tree mapping
--}

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf a) = Leaf $ f a
mapTree f (Branch (Leaf a) t) = Branch (Leaf $ f a) (mapTree f t)
mapTree f (Branch t (Leaf a)) = Branch (mapTree f t) (Leaf $ f a)
mapTree f (Branch t1 t2) = Branch (mapTree f t1) (mapTree f t2)

{--
An example of using recursive DS
https://youtu.be/xAZLceCZGks?t=1158 "Live coding a Mondrian generator" by Ju Liu
--}

{--
https://en.wikibooks.org/wiki/Haskell/Recursion#Don't_get_TOO_excited_about_recursion...
--}











main :: IO ()
main = print "all good!"