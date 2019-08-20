{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UnicodeSyntax #-}

module Harmful where

import Numeric.Natural
import Data.Bool
-- import qualified Data.List as List (foldr)
import Prelude (flip, id, const, (.), (+), (==), (>), undefined, ($))
import Prelude ((<=), (-), Int, Functor, pure, Applicative, fmap)
-- import F

{-
Church Encoding of Data Types
Considered Harmful for Implementations
– Functional Pearl –
https://ifl2014.github.io/submissions/ifl2014_submission_13.pdf
-}

{-
random notes:
http://okmij.org/ftp/tagless-final/course/Boehm-Berarducci.html
- "Boehm-Berarducci isomorphism of algebraic data types and polymorphic lambda-terms"
- "Boehm-Berarducci encoding translates algebraic data types and the operations on them into System F" -- TODO But can we say this /exactly/ for EA
- "Boehm-Berarducci encoding is often confused with Church encoding... Church encoding, which represents data types in an untyped lambda-calculus, is not tight. Without types, it is impossible to separate lambda-terms that encode some data type, from those that represent no data type. The main distinction between the two approaches is subtle. In a word, Church encoding only describes introductions whereas Boehm-Berarducci also define elimination, or pattern-matching, on encoded data types"
-}

-- 4.1
class Pair t where
    pair :: a → b → t a b
    e1   :: t a b → a
    e2   :: t a b → b
-- 4.1.1
instance Pair (,) where
    pair :: a → b → (a, b)
    pair a b = (a, b)
    e1 :: (a, b) → a
    e1 (a, _) = a
    e2 :: (a, b) → b
    e2 (_, b) = b

-- 4.1.2
-- :: FPair a b = FPair (∀t: (a b→t) → t)
-- newtype FPair a b = FPair (forall t. (a → b → t) → t)
newtype FPair a b = FPair   { unPair :: ∀ t. (a → b → t) → t }

swap :: (Pair t) ⇒ t a b → t b a
swap p = pair (e2 p) (e1 p)
{-
swap :: FPair a b → FPair b a
swap (FPair f) = FPair (f . flip)

fromPair :: (a, b) → FPair a b
fromPair (a, b) = FPair (\p → p a b)

curry :: (FPair a b → c) → a → b → c
curry f a b = f (FPair (\g → g a b))

uncurry :: (a → b → c) → FPair a b → c
uncurry f (FPair g) = g f
-}

-- toTuple :: FPair a b → (a, b)
-- -- toTuple (FPair f) = f (\a b → (a, b))
-- toTuple (FPair f) = f (,)

instance Pair FPair where
    pair :: a → b → FPair a b
    pair a b = FPair (\p → p a b)
    e1 :: FPair a b → a
    e1 (FPair p) = p const
    e2 :: FPair a b → b
    e2 (FPair p) = p (flip const)




-- 4.2 Peano Numbers

class Num t where
    zero   :: t
    succ   :: t → t
    isZero :: t → Bool
    pred   :: t → t

-- 4.2.1 Peano Numbers with Integers

instance Num Natural where
    zero     = 0
    succ   n = n + 1
    isZero n = n == 0
    pred   n = if   n > 0
               then n - 1
               else undefined

-- 4.2.2 Peano Numbers with Algebraic Data Types

data Peano = Z | S Peano

instance Num Peano where
    zero     = Z
    succ     = S
    isZero Z = True
    isZero _ = False
    -- isZero n = case n of Z → True
    --                   _    → False
    -- pred   n = case n of (S m) → m
    --                      _     → undefined
    pred (S n) = n
    pred _     = undefined

-- 4.2.3 Peano Numbers in the Church Encoding
-- :: CNum = CNum (∀b: b (b→b) → b)
-- newtype CNum = CNum { getNum :: ∀ a. (a → a) → (a → a) }
newtype CNum = CNum { getNum :: ∀ a. (a → a) → a → a }

suc :: CNum → CNum
suc (CNum n) = CNum (\s → n s . s) -- CNum (\z s → n z (z s))

add :: CNum → CNum → CNum
add (CNum n) (CNum m) = CNum (\s → n s . m s) -- (add n m) s z = n s (m s z)

instance Num CNum where
    zero :: CNum
    zero = CNum (\_ z → z)
    succ :: CNum → CNum
    succ (CNum n) = CNum (\s z → n s (s z)) -- or: \s z → s (n s z)
    isZero :: CNum → Bool
    isZero (CNum n) = n (const False) True
    pred (CNum n) = x
        where (x, _) = n suc (undefined, zero)
              suc (_, b) = (b, succ b)




--
class List t where
    nil   :: t a
    cons  :: a → t a → t a
    isNil :: t a → Bool
    head  :: t a → a
    tail  :: t a → t a

{-
-- this is the version in the paper
instance List [] where
    Nil = []
    Cons a x = [a : x]
    isNil xs = case xs of [] = True
                          _  = False
    head  xs = case xs of [a : x] = a
                          _       = undefined
    tail  xs = case xs of [a : x] = x
                          _       = undefined
-}
instance List [] where
    nil = []
    cons = (:)
    isNil :: [a] → Bool
    isNil [] = True
    isNil _  = False
    head :: [a] → a
    head (a : _) = a
    head _       = undefined
    tail :: [a] → [a]
    tail (_ : x) = x
    tail _       = undefined

-- TODO finish
newtype CList a = CList { foldr :: forall b . (a → b → b) → b → b }
instance List CList where
    nil :: CList a
    nil = CList(\_ n → n)
    cons a v = CList(\c n → c a ((\(CList l) → l) v c n))
    isNil (CList l) = l (\a t → False) True
    head (CList l) = l (\a t → a) undefined
    tail (CList l) = CList (\nil cons → undefined)


