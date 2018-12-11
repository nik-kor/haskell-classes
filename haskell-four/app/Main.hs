module Main where

{--
## Type classes and ad hoc polymorthism

In Haskell data and functions to work with data are separated.
*data* answers question: What does it store?
*class* answers question: What can we do with this data?
*instances* tie both together
--}

class Printable p where
    -- TODO

data Foo = Foo | Bar

instance Printable Foo where
    -- TODO

helloP :: Printable p => p -> String
helloP = undefined

{--
### excercise
implement Printable instance for Int.
Hint: check `show` function
--}

{--
George Wilson - Type Class: The Ultimate Ad Hoc - https://www.youtube.com/watch?v=2EdQFCP5mZ8
--}

{--
## Higher kinded types
--}

{--
Lets explore kinds of types with
:kind
--}

class Box b where -- b :: * -> *
    box :: a -> b a
    unbox :: b a -> a

-- implement instance for Either

{--
### excercise
implement Box instance for Maybe
--}


{--
## Functor
--}

class Functor' f where
    -- TODO

-- implement Functor for Maybe

{--
### excercise
implement functor instance for Identity
--}

newtype Identity a = Identity a


-- TODO implement Reader functor



{--
## Functor laws:
    fmap id = id
    fmap g . fmap f = fmap (g . f)
--}


{--
http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html#just-what-is-a-functor,-really?
--}


main :: IO ()
main = print "all good!"