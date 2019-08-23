{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE ViewPatterns           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UnicodeSyntax          #-}

module RecursionSchemes where

import Prelude hiding (mapM, sequence, replicate, lookup, foldr, length, iterate)
import Control.Applicative (pure, many, empty, (<$>),(<*>),(<*),(*>),(<|>),(<$))
import Control.Arrow ((&&&),(***),(|||), first, second)
import Control.Monad hiding (mapM, sequence)
import Control.Monad.Reader hiding (mapM, sequence)
import Control.Monad.ST
import Data.Foldable (Foldable)
import qualified Data.Foldable as F

---
import Data.List (break, unfoldr)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe
import Data.Monoid
import Data.Traversable
import Numeric
import Data.Bool (bool)
-- import Data.Functor.Identity

-- Notes mostly from https://youtu.be/Zw9KeP3OzpU
-- (An absolutely fantastic presentation by Tim Williams.)


-- Useful functions
-- ================
{-
-- fan-out or _fork_ specialized to functions
-- (Control.Arrow has more general version)
-- (&&&) :: (a → b) → (a → b') → a → (b, b')
-- (f &&& g) x = (f x, g x)

-- fan-in
(|||) :: (a → b) → (c → b) → Either a c → b
(|||) = either

-- function product
(***) :: (a → b) → (a' → b') → (a, a') → (b, b')
(f *** g) (x, y) = (f x, g y)

-- generalised unzip for functors
funzip :: Functor f => f (a, b) → (f a, f b)
funzip = fmap fst &&& fmap snd
-}

-- Fixed points of Functors
-- ========================
-- An idea from category theory which gives:
-- - data-type generic functions
-- - compositional data
-- Fixed points are represented by the type:

-- | the least fixpoint of functor f
newtype   Fix f = Fix   { unFix   :: f (Fix   f) }
-- | The greatest fixpoint of functor f
newtype Cofix f = Cofix { unCofix :: f (Cofix f) }

-- A functor `f` is a data-type of kind `* → *` together with an fmap function.
-- Fix f ~ f(f(f(f(f... etc.
-- ~ as "isomorphic"

-- The /pattern functor/ `ExprF` represents the structure of type `Expr`
-- The isomorphism between a data-type and its pattern functor
-- type is witnessed by the functions `Fix` and `unFix`

-- We can use type classes and functional dependencies to transparently
-- apply the isomorphism between the unfixed representation and the original fixed type,
-- e.g. `[a]` for lists.
class Functor f => Fixpoint f t | t → f where
   inF  :: f t → t
   outF ::   t → f t
data NatF    r = Succ r | Zero deriving Functor
data ListF a r = C a r | N
instance Functor (ListF a) where
    fmap f N        = N
    fmap f (C x xs) = C x (f xs)


cata :: Fixpoint f t => (f a → a) → t → a
cata alg = alg . fmap (cata alg) . outF

-- ana :: Functor f => (a → f a) → a → Fix f
-- ana coalg = Fix . fmap (ana coalg) . coalg
ana :: Functor f => (a → f a) → a → Cofix f
ana coalg = Cofix . fmap (ana coalg) . coalg

instance Functor f => Fixpoint f (Fix f) where
    inF  = Fix
    outF = unFix
instance Fixpoint (ListF a) [a] where
    inF N          = []
    inF (C x xs)   = x : xs
    outF []        = N
    outF (x : xs)  = C x xs
instance Fixpoint NatF Integer where
    inF Zero           = 0
    inF (Succ n)       = n + 1
    outF n | n > 0     = Succ (n - 1)
           | otherwise = Zero




-- Example: coinductive streams
----------------------------
data StreamF a r = S a r deriving Show
type Stream  a   = Cofix (StreamF a)
instance Functor (StreamF a) where
  fmap f (S x xs) = S x (f xs)

-- stream constructor:
cons :: forall a. a → Cofix (StreamF a) → Cofix (StreamF a)
cons h t = Cofix (S h t)

-- stream deconstructors:
head :: forall a. Cofix (StreamF a) → a
head (unCofix → (S h _ )) = h
tail :: forall a. Cofix (StreamF a) → Cofix (StreamF a)
tail (unCofix → (S _ t)) = t



-- Page 15/17
-- http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.35.7133&rep=rep1&type=pdf
data StreamF' c f = ConsF c (f (StreamF' c f))
headF :: StreamF' c f → c
headF (ConsF c _) = c
tailF :: StreamF' c f → f (StreamF' c f)
tailF (ConsF _ fcs) = fcs
genStreamF :: Functor f => (a → c) → (a → f a) → a → StreamF' c f
genStreamF gamma1 gamma2 a = ConsF (gamma1 a) (fmap (genStreamF gamma1 gamma2) (gamma2 a))


-- the function `iterate` generates an infinite stream using the supplied iterator and seed
iterate :: forall a. (a → a) → a → Stream a
iterate f = ana c where
    c :: a → StreamF a a
    c x = S x (f x)
s1 = iterate (+1) 1


-- Example: a simple expression language
-------------------------------------
data ExprF r = Const Int
             | Var   String
             | Add   r r
             | Mul   r r
             | IfNeg r r r
               deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

type Expr = Fix ExprF
{-
--------------------------------------
Composing Algebras
==================
- It is **not** true in general that catamorphisms compose
- However, there is a very useful special case!

Example: an optimisation pipeline
---------------------------------
-}
optAdd :: ExprF Expr → Expr
optAdd (Add (Fix (Const 0)) e              ) = e
optAdd (Add e               (Fix (Const 0))) = e
optAdd e                                     = Fix e
optMul :: ExprF Expr → Expr
optMul (Mul (Fix (Const 1)) e              ) = e
optMul (Mul e               (Fix (Const 1))) = e
optMul e                                     = Fix e

-- The following composition works, but involves two complete traversals:
optimiseSlow :: Expr → Expr
optimiseSlow = cata optAdd . cata optMul

-- We need an algebra composition operator that gives us *short-cut fusion*:
-- cata f . cata g = cata (f `comp` g)


-- For the special case:
-- f :: f a → a;  g :: g (Fix f) → Fix f
-- for arbitrary functors `f` and `g`, this is simply:
-- `comp x y = x . unFix . y`
----
-- We can now derive a more efficient optimise pipeline:\footnote{In practice, such a pipeline is likely to be iterated until an equality fixpoint is reached, hence efficiency is important.}
optimiseFast :: Expr → Expr
optimiseFast = cata (optMul . unFix . optAdd)

-- We have just applied the *catamorphism compose law* [3], usually stated in the form:
{-
f :: f a → a
h :: g a → f a
cata f . cata (Fix . h) = cata (f . h)
-}
