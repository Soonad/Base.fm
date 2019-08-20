{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where -- BigNum where

import Data.List as List
import Data.List.NonEmpty as NE
import Data.Maybe
import GHC.Natural
import Data.Word
-- import Data.Functor.Foldable
import Control.Monad (guard)
import Control.Arrow ((&&&))
import Data.Tuple (swap)

main :: IO ()
main = undefined -- someFunc

type ℕ = Natural

data Digit where
  Zero  :: Digit
  One   :: Digit
  Two   :: Digit
  Three :: Digit
  Four  :: Digit
  Five  :: Digit
  Six   :: Digit
  Seven :: Digit
  Eight :: Digit
  Nine  :: Digit

deriving instance Enum Digit
deriving instance Bounded Digit
instance Show Digit where
  show = show . fromEnum

fromDigit :: (Integral a) => Digit → a
fromDigit Zero  = 0
fromDigit One   = 1
fromDigit Two   = 2
fromDigit Three = 3
fromDigit Four  = 4
fromDigit Five  = 5
fromDigit Six   = 6
fromDigit Seven = 7
fromDigit Eight = 8
fromDigit Nine  = 9

-- Experimenting with a few snippets from:
-- https://stackoverflow.com/questions/3963269/split-a-number-into-its-digits-with-haskell
-- https://stackoverflow.com/questions/1918486/convert-list-of-integers-into-one-int-like-concat-in-haskell/1918522
-- https://stackoverflow.com/questions/49135351/deforestation-in-a-hylomorphism?rq=1
-- https://kodu.ut.ee/~varmo/papers/thesis.pdf
{-
So there are two primary considerations I see for representing numbers as unfolds:
- 
- 
-}

-- TODO put into unfold form
toDigits :: Integral a => a → [a]
toDigits 0 = []
-- essentially need a difference list or to reverse the list at the end
toDigits x = toDigits r ++ [l]
      where (l, r) = swap (quotRem x 10) -- (quotRem x 10)

--
toDigits' :: Integer → [Integer]
toDigits' = List.reverse . List.unfoldr go
  where go :: Integer → Maybe (Integer, Integer)
        -- I wish they made the effort to put in a few details, this is actually a pretty neat solution!
        go = uncurry (*>) . (&&&) (guard . (>0)) (Just . swap . (`quotRem` 10))

-- TODO make foldr version
fromDigits' :: [Integer] → Integer
fromDigits' = foldl (\n m → (10 * n) + m) 0

type Number = [Digit]



-- https://www.reddit.com/r/haskell/comments/14wkkw/function_snoc_list_is_it_implemented_yet/
data FList f a where
    FNil   :: f a → FList f a
    FSnoc  :: f b → FList f (b → a) → FList f a
instance (Functor f) => Functor (FList f) where
    fmap f (FNil   e)    = FNil (fmap f e)
    fmap f (FSnoc  e p)  = FSnoc e  (fmap (f .) p)
instance (Applicative f) => Applicative (FList f) where
    pure = FNil . pure
    prev <*> (FNil e)     = FSnoc e prev
    prev <*> (FSnoc e p)  = FSnoc e (fmap (.) prev <*> p)

