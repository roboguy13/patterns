{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

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
  (:->) :: Pattern ADT s t -> (t -> ADT r) -> ADT (PatFn t r)
  (:|) :: ADT (PatFn a r) -> ADT (PatFn b r) -> ADT (PatFn (Either a b) r)

  Apply :: (a --> b) -> ADT a -> ADT b

  Nil' :: ADT [a]
  Cons' :: ADT a -> ADT [a] -> ADT [a]

  -- Rec :: ADT (RecLayer ADT [a]) -> ADT [a]

  TailRec :: ((a --> b) -> (a --> b)) -> ADT (PatFn a b)

type x --> y = ADT (PatFn (ERepTy x) y)

data Pattern f s t where
  BasePat :: Pattern f (f a) a
  PairPat :: Pattern f (f (a, b)) (f a, f b)
  InLPat :: Pattern f (f (Either a b)) (f a)
  InRPat :: Pattern f (f (Either a b)) (f a)
  CompPat :: Pattern f a b -> Pattern f b c -> Pattern f a c
  -- MatchRec :: ... ?

(.->) :: Pattern ADT s t -> (t -> ADT r) -> ADT (PatFn t r)
(.->) = (:->)

(.|) :: ADT (PatFn a r) -> ADT (PatFn b r) -> ADT (PatFn (Either a b) r)
(.|) = (:|)

type family ERepTy t
type instance ERepTy [a] = Either () (ADT a, ADT [a])
type instance ERepTy (a, b) = (a, b)
type instance ERepTy (Either a b) = Either a b
type instance ERepTy () = ()

pattern NilPat  = CompPat InLPat BasePat
pattern ConsPat = CompPat InRPat PairPat

adtSum :: [Int] --> Int
adtSum = TailRec $ \rec ->
  (NilPat  .-> \()      -> Value 0) .|
  (ConsPat .-> \(x, xs) -> Add x (Apply rec xs))

runMatch :: ([a] --> b) -> ADT [a] -> ADT b
runMatch (CompPat InLPat BasePat :-> f) Nil' = f (Left ())
runMatch (CompPat InRPat PairPat :-> f) (Cons' x xs) = f (Right (x, xs))

-- runMatch (NilPat  :-> f) Nil' = f (Left ())
-- runMatch (ConsPat :-> f) (Cons' x xs) = f (Right (x, xs))

-- lookupMatch :: Pattern ADT s t -> (t --> r) -> (

-- This is @Free@
data RecLayer f a where
  Unwrap :: a -> RecLayer f a
  Preserve :: f (RecLayer f a) -> RecLayer f a

