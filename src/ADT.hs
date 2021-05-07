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
{-# LANGUAGE DefaultSignatures #-}

import           Control.Applicative
import           Data.Kind

import           GHC.Generics

import           ERep
import           Pattern

data ADT t where
  Lit :: Int -> ADT Int
  Add :: ADT Int -> ADT Int -> ADT Int

  Unit :: ADT ()
  Pair :: forall a b. ADT a -> ADT b -> ADT (a, b)
  InL :: forall a b. ADT a -> ADT (Either a b)
  InR :: forall a b. ADT b -> ADT (Either a b)

  -- | Similar to Mark Tullsen's First Class Patterns paper
  (:->) :: (DSL ADT s) => Pattern ADT (ADT s) t -> (t -> ADT r) -> ADT (PatFn s r)
  (:|) :: (Either a b --> r) -> (Either a b --> r) -> (Either a b --> r)

  Apply :: (a --> b) -> ADT a -> ADT b

  Nil :: ADT [a]
  Cons :: ADT a -> ADT [a] -> ADT [a]

  Rec :: ERepTy a ~ a => ((a --> b) -> (a --> b)) -> ADT (PatFn a b)

type x --> y = ADT (PatFn (ERepTy x) y)

-- TODO: Look into using Template Haskell to automatically generate pattern
-- synonyms like these
pattern NilPat  = InLPat
pattern ConsPat = CompPat InRPat PairPat

class ERep t => DSL f t where
  toDSL :: ERepTy t -> f t
  dslEmbed :: f t -> f (ERepTy t)

  default dslEmbed :: ERepTy t ~ t => f t -> f (ERepTy t)
  dslEmbed = id

instance DSL ADT Int where
  toDSL = Lit

instance DSL ADT () where
  toDSL _ = Unit

instance (DSL ADT a, DSL ADT b) => DSL ADT (a, b) where
  toDSL (x, y) = Pair (toDSL (rep x)) (toDSL (rep y))

instance (DSL ADT a, DSL ADT b) => DSL ADT (Either a b) where
  toDSL (Left x)  = InL (toDSL (rep x))
  toDSL (Right y) = InR (toDSL (rep y))

instance (DSL ADT a, ERep a) => DSL ADT [a] where
  toDSL (Left ())       = Nil
  toDSL (Right (x, xs)) = Cons (toDSL (rep x)) (toDSL (rep xs))

  dslEmbed Nil = InL Unit
  dslEmbed (Cons x xs) = InR (Pair x xs)

adtSum :: [Int] --> Int
adtSum = Rec $ \rec ->
  (NilPat  :-> \Unit    -> Lit 0) :|
  (ConsPat :-> \(x, xs) -> Add x (Apply rec xs))

-- runMatch :: (DSL ADT a, DSL ADT b) => (a --> b) -> ADT a -> Maybe b
runMatch :: (DSL ADT a, DSL ADT b) => ADT (PatFn (ERepTy a) b) -> ADT a -> Maybe b
runMatch (BasePat :-> f) arg = Just $ eval $ f (dslEmbed arg)
runMatch (PairPat :-> f) arg =
  case dslEmbed arg of
    Pair x y -> Just $ eval $ f (x, y)
    _ -> Nothing

runMatch (InLPat :-> f) arg =
  case dslEmbed arg of
    InL x -> Just $ eval $ f x
    _ -> Nothing

runMatch (InRPat :-> f) arg =
  case dslEmbed arg of
    InR y -> Just $ eval $ f y
    _ -> Nothing

runMatch (p :| q) arg = runMatch p arg <|> runMatch q arg
runMatch _ _ = Nothing

listToCanonical :: [Int] -> ERepTy [Int]
listToCanonical [] = Left ()
listToCanonical (x:xs) = Right (x, xs)

eval :: forall a. ADT a -> a
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval x@(Rec f) = eval (f x)
eval x@(_ :| _) = PatFn $ \z -> runMatch x (toDSL z)

testList :: ADT [Int]
testList = Cons (Lit 1) (Cons (Lit 2) (Cons (Lit 3) Nil))

-- lookupMatch :: Pattern ADT s t -> (t --> r) -> (

-- TODO: Use this type to test things out
data Three a = Three a a a deriving (Generic)
instance ERep a => ERep (Three a)

