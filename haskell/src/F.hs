{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module F where

import Control.Applicative
-- import Control.Monad ((>=>), (=<<))
import Prelude (flip, id, const, (.), (+), (==), undefined, ($), (>>=))
import Numeric.Natural
import qualified Data.List as List (foldr)
import Data.Bool (Bool, Bool (True, False))
import qualified Data.Bool as Bool (bool, not)
import Data.Maybe (Maybe, Maybe (Nothing, Just))
import qualified Data.Maybe as Maybe (maybe)
import Data.Either (Either, Either (Left, Right))
import qualified Data.Either as Either (either)


import Prelude (Monad, return, Functor)

-- Lambda encodings of `Bool`, `Either a b`, `(a, b)`, `Maybe a`
-- Because the types here do not involve (co-)recursion, the different lambda encodings (Church, Scott, etc.) are the same
newtype FBool         = FBool   { unBool   :: forall a. a → a → a }
newtype FEither   a b = FEither { unEither :: forall c. (a → c) → (b → c) → c }
newtype FPair     a b = FPair   { unPair   :: forall c. (a → b → c) → c }
newtype FMaybe    a   = FMaybe  { unMaybe  :: forall b. b → (a → b) → b }

unbool :: a → a → FBool → a
unbool t f (FBool bool) = bool t f

fromBool :: Bool → FBool
fromBool False = false
fromBool True  = true

toBool :: FBool → Bool
toBool (FBool bool) = bool True False


-- "first" semigroup
true :: FBool
true  = FBool (\t f → t)
-- true = FBool const

-- "last" semigroup
false :: FBool
false = FBool (\t f → f)

not :: FBool → FBool
not (FBool b) = FBool (\t f → b f t)
-- not (FBool b) = FBool (flip b)


-- (&&)
and :: FBool → FBool → FBool
and (FBool a) (FBool b) = FBool (\t f → a (b t f) f)
-- and (FBool a) (FBool b) = FBool (\t → b t >>= a)
-- and (FBool a) (FBool b) = FBool (\t → a =<< b t)
-- and (FBool a) (FBool b) = FBool ((a =<<) . b)
-- and (FBool a) (FBool b) = FBool (b >=> a)

-- (||)
or :: FBool → FBool → FBool
or (FBool a) (FBool b) = FBool (\t f → a t (b t f))
-- or' (FBool a) (FBool b) = FBool (\t → a t . (b t))
-- or' (FBool a) (FBool b) = FBool (liftA2 (.) a b)

xor :: FBool → FBool → FBool
xor (FBool a) (FBool b) = FBool (\t f → a (b f t) (b t f))

pair :: a → b → FPair a b
pair a b = FPair (\p → p a b)

toPair :: FPair a b → (a, b)
toPair (FPair f) = f (\a b → (a, b))
-- toPair (FPair f) = f (,)

fromPair :: (a, b) → FPair a b
fromPair (a, b) = FPair (\p → p a b)

fst :: FPair a b → a
fst (FPair p) = p const

snd :: FPair a b → b
snd (FPair p) = p (flip const)

swap :: FPair a b → FPair b a
swap (FPair f) = FPair (f . flip)

curry :: (FPair a b → c) → a → b → c
curry f a b = f (FPair (\g → g a b))

uncurry :: (a → b → c) → FPair a b → c
uncurry f (FPair g) = g f


just :: a → FMaybe a
just a = FMaybe (\_ f → f a)

toMaybe :: FMaybe a → Maybe a
toMaybe (FMaybe maybe) = maybe Nothing Just

fromJust :: FMaybe a → a
fromJust (FMaybe maybe) = maybe undefined id

fromMaybe :: Maybe a → FMaybe a
fromMaybe m = FMaybe (\b f → Maybe.maybe b f m)

isJust :: FMaybe a → Bool
isJust (FMaybe maybe) = maybe False (const True)

isNothing :: FMaybe a → Bool
isNothing (FMaybe maybe) = maybe True (const False)
-- isNothing = Bool.not . isJust

-- append :: FMaybe a → FMaybe a → FMaybe a
-- append (FMaybe m1) (FMaybe m2) = FMaybe _

maybeToList :: FMaybe a → [a]
-- maybeToList (FMaybe f) = f [] (\a → [a])
-- maybeToList (FMaybe f) = f [] pure
maybeToList (FMaybe maybe) = maybe [] (: [])

either :: (a → c) → (b → c) → FEither a b → c
either ac bc (FEither e) = e ac bc

toEither :: FEither a b → Either a b
toEither (FEither f) = f Left Right

fromEither :: Either a b → FEither a b
fromEither e = FEither (\l r → Either.either l r e)

isLeft :: FEither a b → Bool
isLeft (FEither e) = e (const True) (const False)

fromLeft :: a → FEither a b → a
fromLeft a (FEither e) = e id (const a)

isRight :: FEither a b → Bool
isRight = Bool.not . isLeft

fromRight :: b → FEither a b → b
fromRight b (FEither e) = e (const b) id

left :: a → FEither a b
left a = FEither (\ac _ → ac a)

right :: b → FEither a b
right b = FEither (\_ bc → bc b)

map :: (a → b) → FEither c a → FEither c b
-- map f (FEither e) = e (\c → left c) (\a → right (f a))
-- map f (FEither e) = e left           (\a → right (f a))
map f (FEither e) = e left              (right . f)


bimapE :: (a → b) → (c → d) → FEither a c → FEither b d
-- bimap ab cd (FEither e) = e (\a → left (ab a)) (\c → right (cd c))
bimapE ab cd (FEither e) = e (left . ab) (right . cd)

bimapP :: (a → b) → (c → d) → FPair a c → FPair b d
bimapP ab cd (FPair p) = p (\a c → pair (ab a) (cd c))

bool_match :: Bool → a → a → a
bool_match x true false = case x of { True → true; False → false }

data Pair a b = Pair a b

pair_match :: Pair a b → (a → b → c) → c
pair_match x pair = case x of { Pair a b → pair a b }

