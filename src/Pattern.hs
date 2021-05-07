{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

import           Data.Functor.Foldable
import           Data.Kind

-- | Like the pattern combinators from Mark Tullsen's First Class Patterns
-- paper
newtype PatFn a b = PatFn { runPatFn :: a -> Maybe b }

data ADT t where
  Value :: t -> ADT t
  -- Pair :: ADT a -> ADT b -> ADT (a, b)
  -- InL :: forall a b. ADT a -> ADT (Either a b)
  -- InR :: forall a b. ADT b -> ADT (Either a b)

  Add :: ADT Int -> ADT Int -> ADT Int

  -- | Similar to Mark Tullsen's First Class Patterns paper
  (:->) :: Pattern ADT (ADT s) t -> (t -> ADT r) -> ADT (PatFn s r)
  (:|) :: ADT (PatFn (Either a b) r) -> ADT (PatFn (Either a b) r) -> ADT (PatFn (Either a b) r)
  -- (:|) :: ADT (PatFn a r) -> ADT (PatFn b r) -> ADT (PatFn (Either a b) r)

  Apply :: (a --> b) -> ADT a -> ADT b

  Nil' :: ADT [a]
  Cons' :: ADT a -> ADT [a] -> ADT [a]

  -- Rec :: ADT (RecLayer ADT [a]) -> ADT [a]

  Rec :: ((a --> b) -> (a --> b)) -> ADT (PatFn a b)

type x --> y = ADT (PatFn (ERepTy x) y)

data Pattern f s t where
  BasePat :: Pattern f (f a) a
  PairPat :: Pattern f (f (a, b)) (f a, f b)
  InLPat :: Pattern f (f (Either a b)) (f a)
  InRPat :: Pattern f (f (Either a b)) (f b)
  CompPat :: Pattern f a b -> Pattern f b c -> Pattern f a c
  -- MatchRec :: ... ?

(.->) :: Pattern ADT (ADT s) t -> (t -> ADT r) -> ADT (PatFn s r)
(.->) = (:->)

-- (.|) :: ADT (PatFn a r) -> ADT (PatFn b r) -> ADT (PatFn (Either a b) r)
(.|) :: ADT (PatFn (Either a b) r) -> ADT (PatFn (Either a b) r) -> ADT (PatFn (Either a b) r)
(.|) = (:|)

type family ERepTy t
type instance ERepTy [a] = Either () (a, ADT [a])
type instance ERepTy (a, b) = (a, b)
type instance ERepTy (Either a b) = Either a b
type instance ERepTy () = ()

--NilPat :: Pattern f (f (Either t b2))
-- pattern NilPat :: Pattern ADT (ADT (Either () (a, [a]))) ()
pattern NilPat  = CompPat InLPat BasePat

--Pattern f (f (Either (a2, b3) b2)) (f a2, f b3)
--Pattern f (f (Either a1 (a2, b3))) (f a2, f b3)
-- pattern ConsPat :: Pattern ADT (ADT (Either () (a, [a]))) (ADT a, ADT [a])
pattern ConsPat = CompPat InRPat PairPat

adtSum :: [Int] --> Int
adtSum = Rec $ \rec ->
  (NilPat  .-> \()      -> Value 0) .|
  (ConsPat .-> \(x, xs) -> Add x (Apply undefined xs))

  -- (ConsPat .-> \(x, xs) -> Add x (Apply rec xs)) .|
  -- (NilPat  .-> \()      -> Value 0)

-- runMatch :: ([a] --> b) -> ADT [a] -> ADT b
runMatch :: ERepTy [a] ~ Either () (ADT a, ADT [a]) => ADT (PatFn (ERepTy [a]) b) -> ADT [a] -> ADT b
runMatch = undefined
-- runMatch (CompPat InLPat BasePat :-> f) Nil' = f (Left ())
-- runMatch (ConsPat :-> f) (Cons' x xs) = _ --f (Right (x, xs))
-- runMatch (ConsPat :-> f) (Cons' x xs) = f (x, xs)

-- runMatch (NilPat  :-> f) Nil' = f (Left ())
-- runMatch (ConsPat :-> f) (Cons' x xs) = f (Right (x, xs))

-- lookupMatch :: Pattern ADT s t -> (t --> r) -> (

-- This is @Free@
data RecLayer f a where
  Unwrap :: a -> RecLayer f a
  Preserve :: f (RecLayer f a) -> RecLayer f a

