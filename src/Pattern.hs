{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Pattern where

import           ERep

import           GHC.Types

-- | Like the pattern combinators from Mark Tullsen's First Class Patterns
-- paper
newtype PatFn a b = PatFn { runPatFn :: a -> Maybe b }

-- | Non-nested patterns ("simple patterns")
data Pattern p (f :: Type -> Type) s t where
  BasePat :: Pattern p f (p f a) a
  PairPat :: Pattern p f (p f (a, b)) (p f a, p f b)
  InLPat :: Pattern p f (p f (Either a b)) (p f a)
  InRPat :: Pattern p f (p f (Either a b)) (p f b)
  CompPat :: Pattern p f a (p f b) -> Pattern p f (p f b) c -> Pattern p f a c
  -- MatchRec :: ... ?

data SomePattern p f s = forall t. SomePattern (Pattern p f s t)

someInLPat :: forall p f a b. SomePattern p f (p f a) -> SomePattern p f (p f (Either a b))
someInLPat (SomePattern p) = SomePattern (CompPat InLPat p)

someInRPat :: forall p f a b. SomePattern p f (p f b) -> SomePattern p f (p f (Either a b))
someInRPat (SomePattern p) = SomePattern (CompPat InRPat p)

somePairPat :: forall p f a b. SomePattern p f (p f a) -> SomePattern p f (p f b) -> SomePattern p f (p f (a, b))
somePairPat (SomePattern p) (SomePattern q) = SomePattern PairPat

class GetPatterns p f a where
  getPatterns' :: [SomePattern p f (p f a)]

instance GetPatterns p f () where
  getPatterns' = [SomePattern BasePat]

instance (GetPatterns p f a, GetPatterns p f b) => GetPatterns p f (Either a b) where
  getPatterns' = map someInLPat (getPatterns' @p @f @a)
                ++ map someInRPat (getPatterns' @p @f @b)

instance (GetPatterns p f a, GetPatterns p f b) => GetPatterns p f (a, b) where
  getPatterns' = [SomePattern PairPat]

instance {-# OVERLAPPABLE #-} GetPatterns p f a where
  getPatterns' = []

getPatterns :: forall p f a. (GetPatterns p f (ERepTy a), ERep a) => [SomePattern p f (p f (ERepTy a))]
getPatterns = getPatterns'
