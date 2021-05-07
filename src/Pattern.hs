{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}

import           Data.Functor.Foldable
import           Data.Kind

-- | Like the pattern combinators from Mark Tullsen's First Class Patterns
-- paper
newtype PatFn a b = PatFn { runPatFn :: a -> Maybe b }

data ADT t where
  Value :: t -> ADT t
  Pair :: ADT a -> ADT b -> ADT (a, b)
  InL :: forall a b. ADT a -> ADT (Either a b)
  InR :: forall a b. ADT b -> ADT (Either a b)

  Add :: ADT Int -> ADT Int -> ADT Int

  (:->) :: Pattern ADT s t -> (t -> ADT r) -> ADT (PatFn t r)
  (:|) :: ADT (PatFn a r) -> ADT (PatFn b r) -> ADT (PatFn (Either a b) r)

  Apply :: (a --> b) -> ADT a -> ADT b

  Nil' :: ADT [a]
  Cons' :: ADT a -> RecLayer ADT [a] -> ADT [a]

  -- Match :: ADT t ->

  Rec :: ADT (RecLayer ADT [a]) -> ADT [a]

type x --> y = ADT (PatFn (ERepTy x) y)

-- data ADTMatch (p :: Type -> Type -> Type) a b where
--   ADTMatchPair :: ADTMatch (,) a b
--   ADTMatchInL :: ADTMatch Either a b
--   -- ADTMatchRec ... ?

data Pattern f s t where
  BasePat :: Pattern f (f a) a
  PairPat :: Pattern f (f (a, b)) (f a, f b)
  InLPat :: Pattern f (f (Either a b)) (f a)
  InRPat :: Pattern f (f (Either a b)) (f a)
  CompPat :: Pattern f a b -> Pattern f b c -> Pattern f a c
  -- MatchRec :: ... ?

-- data Match t where
--   PairMatch :: Pai

-- | Similar to Mark Tullsen's First Class Patterns paper
(.->) :: Pattern ADT s t -> (t -> ADT r) -> ADT (PatFn t r)
(.->) = undefined

(.|) :: ADT (PatFn a r) -> ADT (PatFn b r) -> ADT (PatFn (Either a b) r)
(.|) = undefined

type family ERepTy t
type instance ERepTy [a] = Either () (ADT a, ADT [a])
-- type instance ERepTy (ADT [a]) = ADT (Either () (a, ADT [a]))

pattern NilPat  = CompPat InLPat BasePat
pattern ConsPat = CompPat InRPat PairPat

adtSum :: [Int] --> Int
adtSum =
  (NilPat  .-> \()      -> Value 0) .|
  (ConsPat .-> \(x, xs) -> Add x (Apply adtSum xs))


-- This is @Free@
data RecLayer f a where
  Unwrap :: a -> RecLayer f a
  Preserve :: f (RecLayer f a) -> RecLayer f a

-- view :: ADT t -> RecLayer ADT t
-- view = undefined

-- adtSum :: RecLayer ADT t -> ADT Int
-- adtSum (Unwrap _) = Value 0
-- adtSum (Preserve (Cons' x xs)) = Add (adtSum (_ x)) undefined

-- adtSum :: ADT [Int] -> ADT Int
-- adtSum Nil' = Value 0
-- adtSum (Cons' x xs) = Add x (adtSum (Rec _))

{-

-- transformListMatch :: ([ADT a] -> r) -> ADT [a] -> r
-- transformListMatch = undefined

-- transformListMatch :: ([a] -> r) -> ListF a (RecLayer (ListF a) a) -> r
-- transformListMatch = undefined

transformPairMatch :: ((ADT x, ADT y) -> r) -> ADT (x, y) -> r
transformPairMatch f (Pair x y) = f (x, y)

-- | This is making the decision at the "wrong time" (at EDSL "compilation"
-- time rather than "runtime" in the evaluation function)?
transformSumMatch :: (Either (ADT x) (ADT y) -> r) -> ADT (Either x y) -> r
transformSumMatch f (InL y) = f (Left y)
transformSumMatch f (InR x) = f (Right x)

-- transformListMatch :: ([ADT a] -> r) -> ADT ? -> r

-- transformPairMatch :: ((f x, f y) -> r) -> f (x, y) -> r
-- transformPairMatch = undefined

class Match f rep | f -> rep where
  -- match :: f a -> rep a
  matchPair :: f (x, y) -> ((f x, f y) -> r) -> r

  mkPat :: f a -> Pat a (rep a)

  pair :: forall x y. (f x, f y) -> f (x, y)
  inL :: forall x y. f x -> f (Either x y)
  inR :: forall x y. f y -> f (Either x y)

-}

