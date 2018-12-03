module Main where

import Data.List
import Data.Function (on)
{--
## Haskell is like LEGO

An example of sorting lists

--}

data Person = Person { name :: String, age :: Int }
    deriving (Show, Eq, Ord)

p1, p2, p3 :: Person

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












main :: IO ()
main = print "all good!"
