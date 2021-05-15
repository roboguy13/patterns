{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Pattern where

import           ERep

-- | Like the pattern combinators from Mark Tullsen's First Class Patterns
-- paper
newtype PatFn a b = PatFn { runPatFn :: a -> Maybe b }

-- | Non-nested patterns ("simple patterns")
-- TODO: Can we get rid of the 'f' applications in the second type
-- argument?
data Pattern f s t where
  BasePat :: Pattern f (f a) a
  PairPat :: Pattern f (f (a, b)) (f a, f b)
  InLPat :: Pattern f (f (Either a b)) (f a)
  InRPat :: Pattern f (f (Either a b)) (f b)
  CompPat :: Pattern f a (f b) -> Pattern f (f b) c -> Pattern f a c
  -- MatchRec :: ... ?

data SomePattern f = forall s t. SomePattern (Pattern f (f s) t)

{-
data SomePattern f s = forall t. SomePattern (Pattern f s t)

someInLPat :: forall f a b. SomePattern f (f a) -> SomePattern f (f (Either a b))
someInLPat (SomePattern p) = SomePattern (CompPat InLPat p)

someInRPat :: forall f a b. SomePattern f (f b) -> SomePattern f (f (Either a b))
someInRPat (SomePattern p) = SomePattern (CompPat InRPat p)

somePairPat :: forall f a b. SomePattern f (f a) -> SomePattern f (f b) -> SomePattern f (f (a, b))
somePairPat (SomePattern p) (SomePattern q) = SomePattern PairPat

class GetPatterns f a where
  getPatterns' :: [SomePattern f (f a)]

instance GetPatterns f () where
  getPatterns' = [SomePattern BasePat]

instance (GetPatterns f a, GetPatterns f b) => GetPatterns f (Either a b) where
  getPatterns' = map someInLPat (getPatterns' @f @a)
                ++ map someInRPat (getPatterns' @f @b)

instance (GetPatterns f a, GetPatterns f b) => GetPatterns f (a, b) where
  getPatterns' = [SomePattern PairPat]

instance {-# OVERLAPPABLE #-} GetPatterns f a where
  getPatterns' = []

getPatterns :: forall f a. (GetPatterns f (ERepTy a), ERep a) => [SomePattern f (f (ERepTy a))]
getPatterns = getPatterns'
-}

