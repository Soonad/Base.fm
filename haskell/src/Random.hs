{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UnicodeSyntax #-}

{-# LANGUAGE TypeApplications #-}

-- Just random code scratchpad
module Random where



{-
notF :: Functor f => f Bool → f Bool
notF = fmap not
-}
-- test :: Functor f => f Bool
test :: Applicative f => f Bool
test = undefined

hmm :: Functor f => f a → f a
hmm f = fmap id f
-- hmm = fmap id
-- hmm = id -- by functor law
hmm0 :: Functor f => f Bool → f Bool
-- hmm0 = fmap (id @ Bool)
hmm0 = fmap (const True)
{-
Prelude Data.Void> :set -XTypeApplications 
Prelude Data.Void> :t fmap @ ([])
fmap @ ([]) :: (a → b) → [a] → [b]
Prelude Data.Void> :t fmap @ ([]) @ Bool
fmap @ ([]) @ Bool :: (Bool → b) → [Bool] → [b]
Prelude Data.Void> :t fmap @ ([]) Bool

<interactive>:1:13: error:
    Data constructor not in scope: Bool :: a → b
Prelude Data.Void> :t fmap @ _ @ Bool
fmap @ _ @ Bool :: Functor w => (Bool → b) → w Bool → w b
Prelude Data.Void> :t fmap @ ([]) @ Bool
fmap @ ([]) @ Bool :: (Bool → b) → [Bool] → [b]
Prelude Data.Void> :t vacuous
vacuous :: Functor f => f Void → f a
Prelude Data.Void> :t vacuous @ ([]) @ Bool
vacuous @ ([]) @ Bool :: [Void] → [Bool]
Prelude Data.Void> 
-}
{-
<interactive>:1:1: error:
    Pattern syntax in expression context: fmap@([])
    Did you mean to enable TypeApplications?
Prelude Data.Void> :set -XTypeApplications 
Prelude Data.Void> :t fmap @ ([])
fmap @ ([]) :: (a → b) → [a] → [b]
Prelude Data.Void> :t fmap @ ([]) @ Bool
fmap @ ([]) @ Bool :: (Bool → b) → [Bool] → [b]
Prelude Data.Void> :t fmap @ ([]) Bool

<interactive>:1:13: error:
    Data constructor not in scope: Bool :: a → b
Prelude Data.Void> :t fmap @ _ @ Bool
fmap @ _ @ Bool :: Functor w => (Bool → b) → w Bool → w b
Prelude Data.Void> :t fmap @ ([]) @ Bool
fmap @ ([]) @ Bool :: (Bool → b) → [Bool] → [b]

-}
