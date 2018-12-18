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
fishOperator f g a = undefined

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

-- TBD Monad type class

-- TBD monad instance for Maybe'

{--
## An example of monadic calculation with maybe
--}

checkRange :: Int -> Maybe Int
checkRange x = undefined

safeDiv :: Int -> Int -> Maybe Int
safeDiv x y = undefined

monadicValue :: Maybe (Int, Int, Int)
monadicValue = undefined

{--
## Do notation as a syntactic sugar
--}

monadicValueWithDo :: Maybe (Int, Int, Int)
monadicValueWithDo = undefined













{--
* Bartosz Milewski about Monads https://youtu.be/PlFgKV0ZXoE
* Brian Beckman: Don't fear the Monad https://www.youtube.com/watch?v=ZhuHCtR3xq8
--}

main :: IO ()
main = print "all good!"