{-# LANGUAGE FlexibleContexts, TemplateHaskell, GADTs, TypeOperators #-}

import Control.Monad.Operational.Mini

data Blah b a where
    Foo :: Int -> Blah b Int
    Bar :: b -> Blah b ()
    Baz :: Blah Int a
    Qux :: Blah b b
    Quux :: Blah b a -> Blah b a

makeSingletons ''Blah
