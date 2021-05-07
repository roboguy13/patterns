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

data Match s t

data ADT t where
  Lit :: Int -> ADT Int
  Add :: ADT Int -> ADT Int -> ADT Int

  Unit :: ADT ()
  Pair :: forall a b. ADT a -> ADT b -> ADT (a, b)
  InL :: forall a b. ADT a -> ADT (Either a b)
  InR :: forall a b. ADT b -> ADT (Either a b)

  -- | Similar to Mark Tullsen's First Class Patterns paper
  (:->) :: (DSL ADT s, DSL ADT r, IsCanonical s) =>
    Pattern ADT (ADT s) t -> (t -> ADT r) -> ADT (PatFn s r)

  (:|) :: (DSL ADT a, DSL ADT b, DSL ADT r) =>
    ((Either a b) --> r) -> ((Either a b) --> r) -> ((Either a b) --> r)

  Apply :: ERep a => (ERepTy a --> b) -> ADT a -> ADT b

  Nil :: ADT [a]
  Cons :: ADT a -> ADT [a] -> ADT [a]

  Rec :: ERepTy a ~ a => ((ERepTy a --> b) -> (ERepTy a --> b)) -> ADT (PatFn a b)

-- type x --> y = ADT (PatFn (ERepTy x) y)
type x --> y = ADT (PatFn x y)

-- TODO: Look into using Template Haskell to automatically generate pattern
-- synonyms like these
pattern NilPat  = CompPat InLPat BasePat
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

adtSum :: ERepTy [Int] --> Int
adtSum = Rec $ \rec ->
  (NilPat  :-> \()      -> Lit 0) :|
  (ConsPat :-> \(x, xs) -> Add x (Apply rec xs))

listToInt :: ERepTy [Int] --> Int
listToInt =
  (NilPat  :-> \()    -> Lit 0) :|
  (ConsPat :-> \(_,_) -> Lit 1)

fromPattern :: Pattern ADT (ADT s) t -> ADT s -> Maybe t
fromPattern BasePat arg = Just $ eval arg
fromPattern PairPat (Pair x y) = Just (x, y)
fromPattern InLPat  (InL x) = Just x
fromPattern InRPat  (InR y) = Just y
fromPattern (CompPat p q) arg = do
  x <- fromPattern p arg
  fromPattern q x
fromPattern _ _ = Nothing

matchPattern :: Pattern ADT (ADT s) t -> (t -> ADT r) -> ADT s -> Maybe (ADT r)
matchPattern pat f arg = f <$> fromPattern pat arg

runMatch :: (DSL ADT a, DSL ADT b, IsCanonical a) => ADT (PatFn a b) -> ADT a -> Maybe b
runMatch (pat :-> f) arg = eval <$> matchPattern pat f arg
runMatch (p :| q) arg = runMatch p arg <|> runMatch q arg

listToCanonical :: [Int] -> ERepTy [Int]
listToCanonical [] = Left ()
listToCanonical (x:xs) = Right (x, xs)

eval :: forall a. ADT a -> a
eval (Lit x) = x
eval Unit = ()
eval (InL x) = Left (eval x)
eval (InR y) = Right (eval y)
eval (Pair x y) = (eval x, eval y)
eval Nil = []
eval (Cons x xs) = eval x : eval xs

eval (Add x y) = eval x + eval y
eval x@(Rec f) = eval (f x)
eval x@(_ :| _) = PatFn $ \ z -> runMatch x (toDSL (rep z))
eval x@(_ :-> _) = PatFn $ \ z -> runMatch x (toDSL (rep z))
eval (Apply f x) =
  case runPatFn (eval f) (rep (eval x)) of
    Nothing -> error "eval: Missing pattern match in DSL code"
    Just r -> r

testList :: ADT [Int]
testList = Cons (Lit 1) (Cons (Lit 2) (Cons (Lit 3) Nil))

-- lookupMatch :: Pattern ADT s t -> (t --> r) -> (

-- TODO: Use this type to test things out
data Three a = Three a a a deriving (Generic)
instance ERep a => ERep (Three a)

