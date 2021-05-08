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
{-# LANGUAGE StandaloneDeriving #-}

import           Control.Applicative
import           Data.Kind

import           GHC.Generics

import           ERep
import           Pattern

-- data Match s t where
--   (:-->) :: (DSL ADT a, DSL ADT b, DSL ADT r) =>
--     Pattern ADT (ADT s) t -> (t -> ADT r) -> 

data ADT f t where
  Base :: f a -> ADT f a
  Unit' :: ADT f ()
  Pair' :: forall f a b. ADT f a -> ADT f b -> ADT f (a, b)
  InL' :: forall f a b. ADT f a -> ADT f (Either a b)
  InR' :: forall f a b. ADT f b -> ADT f (Either a b)

pattern Unit = ADT Unit'
pattern Pair x y = ADT (Pair' x y)
pattern InL x = ADT (InL' x)
pattern InR y = ADT (InR' y)

data E t where
  Lit :: Int -> E Int
  Add :: E Int -> E Int -> E Int
  ADT :: ADT E t -> E t

  -- | Similar to Mark Tullsen's First Class Patterns paper
  (:->) :: (DSL E s, DSL E r, IsCanonical s) =>
    Pattern E (E s) t -> (t -> E r) -> E (PatFn s r)

  (:|) :: (DSL E a, DSL E b, DSL E r) =>
    ((Either a b) --> r) -> ((Either a b) --> r) -> ((Either a b) --> r)

  Apply :: ERep a => (ERepTy a --> b) -> E a -> E b

  Nil :: E [a]
  Cons :: E a -> E [a] -> E [a]

  Rec :: ERepTy a ~ a => ((ERepTy a --> b) -> (ERepTy a --> b)) -> E (PatFn a b)

-- type x --> y = ADT (PatFn (ERepTy x) y)
type x --> y = E (PatFn x y)

-- TODO: Look into using Template Haskell to automatically generate pattern
-- synonyms like these
pattern NilPat  = CompPat InLPat BasePat
pattern ConsPat = CompPat InRPat PairPat

class ERep t => DSL f t where
  toDSL :: ERepTy t -> f t
  dslEmbed :: f t -> ADT f (ERepTy t)

  default dslEmbed :: ERepTy t ~ t => f t -> ADT f (ERepTy t)
  dslEmbed = Base

instance ERep (ADT f a) where
  type ERepTy (ADT f a) = ADT f a
  rep = id
  unrep = id

instance DSL E Int where
  toDSL = Lit

instance DSL (ADT f) () where
  toDSL _ = Unit'

instance (DSL f a, DSL f b, DSL (ADT f) a, DSL (ADT f) b) => DSL (ADT f) (a, b) where
  toDSL (x, y) = Pair' (toDSL (rep x)) (toDSL (rep y))

instance (DSL f a, DSL f b, DSL (ADT f) a, DSL (ADT f) b) => DSL (ADT f) (Either a b) where
  toDSL (Left x)  = InL' (toDSL (rep x))
  toDSL (Right y) = InR' (toDSL (rep y))

-- instance {- (DSL f a) => -} ERep a => DSL (ADT E) [a] where
--   dslEmbed (Base Nil) = dslEmbed Nil
--     -- case 

instance (DSL f Int) => DSL (ADT f) Int where
  dslEmbed = undefined

----
instance DSL E () where
  toDSL = ADT . toDSL

instance (DSL (ADT E) a, DSL (ADT E) b, DSL E a, DSL E b) => DSL E (a, b) where
  toDSL = ADT . toDSL

instance (DSL (ADT E) a, DSL (ADT E) b, DSL E a, DSL E b) => DSL E (Either a b) where
  toDSL = ADT . toDSL


instance (DSL E a, ERep a) => DSL E [a] where
  toDSL (Left ())       = Nil
  toDSL (Right (x, xs)) = Cons (toDSL (rep x)) (toDSL (rep xs))

  dslEmbed Nil = InL' Unit'
  dslEmbed (Cons x xs) = InR' (Pair' x xs)

adtSum :: ERepTy [Int] --> Int
adtSum = Rec $ \rec ->
  (NilPat  :-> \()      -> Lit 0) :|
  (ConsPat :-> \(x, xs) -> Add x (Apply rec xs))

listToInt :: ERepTy [Int] --> Int
listToInt =
  (NilPat  :-> \()    -> Lit 0) :|
  (ConsPat :-> \(_,_) -> Lit 1)

{-
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
-}
