{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}

import           Control.Applicative
import           Data.Kind

import           GHC.Generics

import           ERep

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

  Rec :: ERepTy a ~ a => ((a --> b) -> (a --> b)) -> ADT (PatFn a b)

type x --> y = ADT (PatFn (ERepTy x) y)

-- | Non-nested patterns ("simple patterns")
data Pattern f s t where
  BasePat :: Pattern f (f a) (f a)
  PairPat :: Pattern f (f (a, b)) (f a, f b)
  InLPat :: Pattern f (f (Either a b)) (f a)
  InRPat :: Pattern f (f (Either a b)) (f b)
  CompPat :: Pattern f a b -> Pattern f b c -> Pattern f a c
  -- MatchRec :: ... ?

data SomePattern f s = forall t. SomePattern (Pattern f s t)

someInLPat :: forall f a b. SomePattern f (f a) -> SomePattern f (f (Either a b))
someInLPat (SomePattern p) = SomePattern (CompPat InLPat p)

someInRPat :: forall f a b. SomePattern f (f b) -> SomePattern f (f (Either a b))
someInRPat (SomePattern p) = SomePattern (CompPat InRPat p)

somePairPat :: forall f a b. SomePattern f (f a) -> SomePattern f (f b) -> SomePattern f (f (a, b))
somePairPat (SomePattern p) (SomePattern q) = SomePattern PairPat

-- TODO: Use this type to test things out
data Three a = Three a a a deriving (Generic)
instance ERep a => ERep (Three a)

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

(.->) :: Pattern ADT (ADT s) t -> (t -> ADT r) -> ADT (PatFn s r)
(.->) = (:->)

-- (.|) :: ADT (PatFn a r) -> ADT (PatFn b r) -> ADT (PatFn (Either a b) r)
(.|) :: ADT (PatFn (Either a b) r) -> ADT (PatFn (Either a b) r) -> ADT (PatFn (Either a b) r)
(.|) = (:|)

-- type family ERepTy t
-- type instance ERepTy [a] = Either () (a, [a])
-- type instance ERepTy (a, b) = (a, b)
-- type instance ERepTy (Either a b) = Either a b
-- type instance ERepTy () = ()

--NilPat :: Pattern f (f (Either t b2))
-- pattern NilPat :: Pattern ADT (ADT (Either () (a, [a]))) ()
pattern NilPat  = InLPat

--Pattern f (f (Either (a2, b3) b2)) (f a2, f b3)
--Pattern f (f (Either a1 (a2, b3))) (f a2, f b3)
-- pattern ConsPat :: Pattern ADT (ADT (Either () (a, [a]))) (ADT a, ADT [a])
pattern ConsPat = CompPat InRPat PairPat

adtSum :: [Int] --> Int
adtSum = Rec $ \rec ->
  (NilPat  .-> \(Value ()) -> Value 0) .|
  (ConsPat .-> \(x, xs)    -> Add x (Apply rec xs))

  -- (ConsPat .-> \(x, xs) -> Add x (Apply rec xs)) .|
  -- (NilPat  .-> \()      -> Value 0)

runMatch :: ([a] --> b) -> ADT [a] -> Maybe (ADT b)
runMatch (NilPat  :-> f) Nil' = Just $ f (Value ())
runMatch (ConsPat :-> f) (Cons' x xs) = Just $ f (x, xs)
runMatch (x :| y) arg = runMatch x arg <|> runMatch y arg

runMatch (Rec f) arg = error "runMatch: Rec" -- TODO: Find a way to do this better
runMatch (Apply f x) arg = error "runMatch: Apply"
-- runMatch (Rec f) arg = do
--   x <-

runMatch _ _ = Nothing

runMatch' :: (a --> b) -> ADT a -> Maybe (ADT b)
-- runMatch' :: ADT (PatFn (ERepTy a) r) -> a -> Maybe (ADT r)
runMatch' = undefined

-- runMatch' :: ([a] --> b) -> ADT [a] -> ADT b
-- runMatch' m x =
--   case runMatch m x of
--     Nothing -> error "runMatch': No matching pattern"
--     Just r -> r

listToCanonical :: [Int] -> ERepTy [Int]
listToCanonical [] = Left ()
listToCanonical (x:xs) = Right (x, xs)

eval :: ADT a -> a
eval (Value x) = x
eval (Add x y) = eval x + eval y
eval x@(Rec f) = eval (f x)
eval x@(_ :| _) = PatFn $ \z -> eval <$> runMatch' x (Value z) -- TODO: Replace Value with something better
-- eval x@(_ :-> _) = PatFn $ \z -> eval <$> (runMatch' x z)
-- eval (Apply f x) = error "Apply"

testList :: ADT [Int]
testList = Cons' (Value 1) (Cons' (Value 2) (Cons' (Value 3) Nil'))

-- runMatch (NilPat  :-> f) Nil' = f (Left ())
-- runMatch (ConsPat :-> f) (Cons' x xs) = f (Right (x, xs))

-- lookupMatch :: Pattern ADT s t -> (t --> r) -> (

-- This is @Free@
data RecLayer f a where
  Unwrap :: a -> RecLayer f a
  Preserve :: f (RecLayer f a) -> RecLayer f a

