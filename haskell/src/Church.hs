{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax #-}

module Church where

import Numeric.Natural
import Data.Bool
import qualified Data.List as List (foldr)
import Prelude (flip, id, const, (.), (+), (==), undefined, ($))
import Prelude ((<=), (-))
import F


-- TODO
-- http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.76.5557&rep=rep1&type=pdf
newtype ChurchNat  = CNat { getCNat :: forall b. (b → b) → (b → b) }

-- Tony Morris' words on `foldr`:
-- http://data.tmorris.net/talks/list-folds/feb8749ff8447c9558f733f9493292a824b414ad/list-folds.pdf
-- We can think of foldr as "constructor replacement"

newtype ChurchList a = CList { getFoldr :: forall b. (a → b → b) → b → b }

nil :: ChurchList a
nil = CList (\_ nil' → nil')

cons :: a → ChurchList a → ChurchList a
cons a (CList fold) = CList (\cons' nil' → cons' a (fold cons' nil'))

foldr :: ChurchList a → (a → b → b) → b → b
foldr (CList fold) = fold

map :: (a → b) → ChurchList a → ChurchList b
-- map f (CList fold) = CList (\cons' nil' → fold (cons' . f) nil')
map f (CList fold) = CList (\cons' → fold (cons' . f))

toList :: ChurchList a → [a]
toList (CList fold) = fold (:) []

fromList :: [a] → ChurchList a
fromList xs = CList (\cons' nil' → List.foldr cons' nil' xs)

length :: ChurchList a → Natural
length (CList fold) = fold (\_ acc → acc + 1) 0

-- TODO probably more efficient way of doing this :)
null :: ChurchList a → Bool
-- null (CList f) = length' (CList f) == 0
null = (0 ==) . length

head :: ChurchList a → a
head (CList fold) = fold const undefined

tail :: ChurchList a → ChurchList a
tail (CList fold) = CList (\cons' nil' → fold (\a g h → h a (g cons')) (const nil') (const id))

zipWith :: (a → b → c) → ChurchList a → ChurchList b → ChurchList c
zipWith f as bs = if null as || null bs
                  then nil :: (ChurchList c)
                  else let ha = head as
                           hb = head bs
                           ta = tail as
                           tb = tail bs
                       in cons (f ha hb) (zipWith f ta tb)

zip :: ChurchList a → ChurchList b → ChurchList (FPair a b)
zip = zipWith pair
