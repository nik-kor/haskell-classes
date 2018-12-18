module Main where

import Control.Monad

{--
# Monads
--}

{--
## Examples

    Maybe a:        a (p->) b           a -> Maybe b
    Reader e a:     a (e->) b           (a, e) -> b         a -> e -> b         a -> (e -> b)
    State s a:      a (s->) b           (a, s) -> (b, s)    a -> s -> (b, s)    a -> (s -> (b, s))
    [a]:            a (i->) b           a -> [b]
    Either e b:     a (f->) b           a -> Either e b

            +-------------------------------+
            |            a -> m b           |
            +-------------------------------+

--}


{--
## Monads are *really* just about composability

    (.)     :: (b -> c)  -> (a -> b)  -> a -> c
    (>=>)   :: (a -> mb) -> (b -> mc) -> a -> mc
--}

{--
    let's implement `>=>` for a list
--}

fishOperator :: (a -> [b]) -> (b -> [c]) -> a -> [c]
fishOperator f g a =
    let
        bs = f a
        css = fmap g bs
    in
        concat css

{--
### We also need an equivalent for `id` - so:

    f . id       = f
    f >=> return = f
--}

{--
### Associativity law
    (f . g) . h     == f . (g . h)
    (f >=> g) >=> h == f >=> (g >=> h)
--}


{--
## So what is technically a monad in Haskell?
--}
--     m :: * -> *
-- class Monad m where
--     return :: a -> m a
--     (>>=) :: m a -> (a -> m b) -> m b

-- TBD monad instance for Maybe'

data Maybe' a = Nothing' | Just' a

instance Functor Maybe' where
    fmap f (Just' a) = Just' $ f a
    fmap _ _ = Nothing'

instance Applicative Maybe' where
    pure = Just'

    Just' f <*> Just' a = Just' $ f a
    _ <*> _ = Nothing'


instance Monad Maybe' where
    return = Just'

--   (>>=) :: m a -> (a -> m b) -> m b
    (>>=) m f = case m of
        Nothing' -> Nothing'
        Just' x -> f x

{--
## An example of monadic calculation with maybe
--}

checkRange :: Int -> Maybe Int
checkRange x =
    if x > 10
    then Nothing
    else Just x

safeDiv :: Int -> Int -> Maybe Int
safeDiv x y =
    if y == 0
    then Nothing
    else Just $ x `div` y

monadicValue :: Maybe (Int, Int, Int)
monadicValue = checkRange 11
    >>= \a -> safeDiv 10 100
    >>= \b -> checkRange 1
    >>= \c -> return (a, b, c)

{--
## Do notation as a syntactic sugar
--}

monadicValueWithDo :: Maybe (Int, Int, Int)
monadicValueWithDo = do
    a <- checkRange 11
    b <- safeDiv 10 100
    c <- checkRange 1

    return (a, b, c)













{--
* Bartosz Milewski about Monads https://youtu.be/PlFgKV0ZXoE
* Brian Beckman: Don't fear the Monad https://www.youtube.com/watch?v=ZhuHCtR3xq8
--}

main :: IO ()
main = print "all good!"