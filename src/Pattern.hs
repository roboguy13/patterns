{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

import           Data.Functor.Foldable
import           Data.Kind

-- | Like the pattern combinators from Mark Tullsen's First Class Patterns
-- paper
newtype Pat a b = Pat { runPat :: a -> Maybe b }

data ADT t where
  Value :: t -> ADT t
  Pair :: ADT a -> ADT b -> ADT (a, b)
  InL :: forall a b. ADT a -> ADT (Either a b)
  InR :: forall a b. ADT b -> ADT (Either a b)

  Add :: ADT Int -> ADT Int -> ADT Int

  Nil' :: ADT [a]
  Cons' :: ADT a -> RecLayer ADT [a] -> ADT [a]

  -- Match :: ADT t ->

  Rec :: ADT (RecLayer ADT [a]) -> ADT [a]

-- data ADTMatch (p :: Type -> Type -> Type) a b where
--   ADTMatchPair :: ADTMatch (,) a b
--   ADTMatchInL :: ADTMatch Either a b
--   -- ADTMatchRec ... ?

data Match f s t where
  MatchBase :: Match f (f a) a
  MatchPair :: Match f (f (a, b)) (f a, f b)
  MatchInL :: Match f (f (Either a b)) (f a)
  MatchInR :: Match f (f (Either a b)) (f a)
  MatchComp :: Match f a b -> Match f b c -> Match f a c
  -- MatchRec :: ... ?

-- data Match t where
--   PairMatch :: Pai

-- | Similar to Mark Tullsen's First Class Patterns paper
(.->) :: Match ADT s t -> (t -> ADT r) -> ADT (t -> Maybe r)
(.->) = undefined

(.|) :: ADT (a -> Maybe r) -> ADT (b -> Maybe r) -> ADT (Either a b -> Maybe r)
(.|) = undefined

type family ERepTy t
type instance ERepTy [a] = Either () (ADT a, ADT [a])
-- type instance ERepTy (ADT [a]) = ADT (Either () (a, ADT [a]))

adtSum :: ADT (ERepTy [Int] -> Maybe Int)
adtSum = (MatchComp MatchInL MatchBase .-> \() -> Value 0)
          .| (MatchComp MatchInR MatchPair .-> \(x, xs) -> Add x _)


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

