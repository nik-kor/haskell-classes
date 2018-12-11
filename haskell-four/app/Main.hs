module Main where

{--
## Type classes and ad hoc polymorthism

In Haskell data and functions to work with data are separated.
*data* answers question: What does it store?
*class* answers question: What can we do with this data?
*instances* tie both together
--}

class Printable p where
    printMe :: p -> String

data Foo = Foo | Bar | Int

instance Printable Foo where
    printMe Foo = "it's foo"
    printMe Bar = "barrrr"

helloP :: Printable p => p -> String
helloP p = "hi there " ++ printMe p

{--
### excercise
implement Printable instance for Int.
Hint: check `show` function
--}
instance Printable Int where
    printMe p = "it was int" ++ show p

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
    -- box :: a -> a
    unbox :: b a -> a

-- implement instance for Either
-- data Either a b = Left a | Right b

instance (Show e) => Box (Either e) where
    box = Right

    unbox (Right a) = a
    unbox (Left b) = error $ show b


{--
### excercise
implement Box instance for Maybe
--}
-- data Maybe a = Nothing | Just a
instance Box Maybe where
    box = Just

    unbox (Just a) = a
    unbox Nothing = error "cannot extract value from nothing"


{--
## Functor
--}

class Functor' f where -- f :: * -> *
    fmap' :: (a -> b) -> f a -> f b

-- implement Functor for Maybe

instance Functor' Maybe where
    fmap' f (Just a) = Just $ f a
    fmap' _ Nothing = Nothing

{--
### excercise
implement functor instance for Identity
--}

newtype Identity a = Identity a deriving (Show)

instance Functor' Identity where
    fmap' f (Identity a) = Identity $ f a

-- TODO implement Reader functor
newtype Reader e a = Reader (e -> a)

instance Functor' (Reader e) where
    -- (a -> b)    (e -> a)    (e -> b)
    fmap' f (Reader g) = Reader $ f . g


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