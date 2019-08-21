{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax #-}

module Scott where

-- import Control.Applicative
import Prelude (flip, id, const, (.), (+), (==), undefined, ($))
import Numeric.Natural
import qualified Data.List as List (foldr)
import Data.Bool (Bool, Bool (True, False))
import qualified Data.Bool as Bool (bool, not)
import Data.Maybe (Maybe, Maybe (Nothing, Just))
import qualified Data.Maybe as Maybe (maybe)
import Data.Either (Either, Either (Left, Right))
import qualified Data.Either as Either (either)
import F

newtype ScottNat = NatS { unNat ∷ forall b . (ScottNat → b) → b }

newtype ScottList a = ListS { unCons ∷ forall b. (a → ScottList a → b) → b → b }

maybeToList ∷ FMaybe a → ScottList a
-- maybeToList (FMaybe maybe) = maybe nil₀ (\a → cons a nil₀)
maybeToList (FMaybe maybe) = maybe nil₀ (`cons₀` nil₀)

catMaybe ∷ ScottList (FMaybe a) → ScottList a
-- catMaybe (ListS unfold) = unfold (\(FMaybe maybe) xs → (maybe id cons) (catMaybe xs)) nil₀
catMaybe (ListS unfold) = unfold (\(FMaybe maybe) → maybe id cons₀ . catMaybe) nil₀

-- case
uncons ∷ (a → ScottList a → b) → b → ScottList a → b
uncons cons nil (ListS f) = f cons nil

nil₀ ∷ ScottList a
nil₀ = ListS (\_ nil → nil)
-- TODO relation to `type Stream a = Cofree Identity a`
pure ∷ a → ScottList a
-- pure a = ListS (\cons nil → cons a _) -- N.B. that we /can't/ actually use `nil` here! (but `nil₀` would work)
-- This works (type checks), but isn't quite the intention
-- pure a = ListS (\cons nil → cons a (pure a)) -- type checks but is a stream :)
pure a = ListS (\cons _ → cons a nil₀)

cons₀ ∷ a → ScottList a → ScottList a
-- cons₀ a as = ListS (\cons _ → cons a as) -- TODO was good one
cons₀ a (ListS unfold) = unfold cons₀ (pure a) -- TODO
  -- ListS (\cons nil → cons a as)
-- cons₀ a (ListS unfold) = unfold (\a' as → cons₀ a (cons₀ a' as)) undefined
-- cons₀ a (ListS unfold) = unfold cons₀ undefined

-- `null (fromList [0 ..])` doesn't terminate
null ∷ ScottList a → Bool
-- null list = uncons (\_ _ → False) True list
null = uncons (\_ _ → False) True
-- null (ListS unfold) = unfold (\_ _ → False) True

map ∷ (a → b) → ScottList a → ScottList b
-- map f (ListS unfold) = ListS (\cons nil → unfold (\a t → cons (f a) (map f t)) nil)
-- map f (ListS unfold) = ListS (\cons nil → unfold (\a → cons (f a) . (map f)) nil)
-- map f (ListS unfold) = ListS (\cons nil → unfold (\a → cons (f a) . map f) nil)
map f (ListS unfold) = ListS (\cons → unfold (\cons' → cons (f cons') . Scott.map f))
-- map f = uncons (\x xs → cons (f x) (map f xs)) nil

length ∷ ScottList a → Natural
length (ListS unfold) = unfold (\_ acc → length acc + 1) 0

toList ∷ ScottList a → [a]
toList = foldr (:) []

fromList ∷ [a] → ScottList a
fromList = List.foldr cons₀ nil₀

append ∷ ScottList a → ScottList a → ScottList a
-- append (ListS f) (ListS g) = ListS (\cons nil → f (\a as → cons a (append as (ListS g)))  (g cons nil))
-- append (ListS unfold₁) (ListS unfold₂) = ListS (\cons nil → unfold₁ (\a as → cons a (append as (ListS unfold₂)))  (unfold₂ cons nil))
-- append (ListS unfold₁) (ListS unfold₂) = ListS (\cons → unfold₁ (\a as → cons a (append as (ListS unfold₂))) . (unfold₂ cons))
-- append (ListS unfold₁) ys@(ListS unfold₂) = ListS (\cons → unfold₁ (\a as → cons a (append as ys)) . (unfold₂ cons))
append = flip (foldr cons₀)

filter ∷ (a → FBool) → ScottList a → ScottList a
filter p (ListS unfold) = unfold (\h t → if toBool (p h) then cons₀ h (filter p t) else filter p t) nil₀

{-
partitionFEither ∷ [Either a b] → ([a], [b])
partitionFEither = foldr (either left right) ([], [])
 where
  left  a ~(l, r) = (a : l,     r)
  right a ~(l, r) = (    l, a : r)
-}
partitionEither ∷ ScottList (FEither a b) → FPair (ScottList a) (ScottList b)
partitionEither = foldr (either left right) (pair nil₀ nil₀)
  where
      left ∷ a → FPair (ScottList a) b → FPair (ScottList a) b
      left  a p = pair (cons₀ a (fst p))          (snd p)
        -- pair (cons₀ a l)        r
      right ∷ b → FPair a (ScottList b) → FPair a (ScottList b)
      -- right b ~(l, r) = pair         l (cons₀ b r)
      right b p = pair          (fst p)  (cons₀ b (snd p))

foldr ∷ (a → b → b) → b → ScottList a → b
-- foldr f b xs = uncons (\a q' → f a (foldr f b q')) b xs
foldr cons nil (ListS unfold) = unfold (\a as → cons a (foldr cons nil as)) nil

foldl ∷ (b → a → b) → b → ScottList a → b
foldl f b xs = foldr (\a g b' → g (f b' a)) id xs b

{-
zipWith ∷ (a → b → c) → ScottList a → ScottList b → ScottList c
zipWith f as bs = if null as || null bs
                   then nil ∷ (ScottList c)
                   else let ha = head as
                            hb = head bs
                            ta = tail as
                            tb = tail bs
                        in cons (f ha hb) (zipWith f ta tb)
-}
zipWith ∷ (a → b → c) → ScottList a → ScottList b → ScottList c
-- zipWith f (ListS unfold₁) (ListS unfold₂) = unfold₁ (\a as → unfold₂ (\b bs → cons₀ (f a b) (zipWith f as bs)) nil) nil
-- zipWith f (ListS unfold₁) (ListS unfold₂) = unfold₁ (\a as → unfold₂ (\b → cons₀ (f a b) . (zipWith f as))  nil) nil
-- zipWith f (ListS unfold₁) (ListS unfold₂) = unfold₁ (\a as → unfold₂ (\b → cons₀ (f a b) . zipWith f as) nil) nil
zipWith f (ListS unfold₁) (ListS unfold₂) = unfold₁ (\a as → unfold₂ (\b → cons₀ (f a b) . zipWith f as) nil₀) nil₀

zip ∷ ScottList a → ScottList b → ScottList (FPair a b)
-- zip (ListS unfold₁) (ListS unfold₂) = unfold₁ (\a as → unfold₂ (\b bs → cons (pair a b) (zip as bs))  nil) nil
-- zip (ListS unfold₁) (ListS unfold₂) = unfold₁ (\a as → unfold₂ (\b → cons (pair a b) . zip as)  nil) nil
zip = zipWith pair
