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
import           Pattern

data ADT t where
  Value :: t -> ADT t
  Add :: ADT Int -> ADT Int -> ADT Int

  -- | Similar to Mark Tullsen's First Class Patterns paper
  (:->) :: Pattern ADT (ADT s) t -> (t -> ADT r) -> ADT (PatFn s r)
  (:|) :: (Either a b --> r) -> (Either a b --> r) -> (Either a b --> r)

  Apply :: (a --> b) -> ADT a -> ADT b

  Nil' :: ADT [a]
  Cons' :: ADT a -> ADT [a] -> ADT [a]

  Rec :: ERepTy a ~ a => ((a --> b) -> (a --> b)) -> ADT (PatFn a b)

type x --> y = ADT (PatFn (ERepTy x) y)

(.->) :: Pattern ADT (ADT s) t -> (t -> ADT r) -> ADT (PatFn s r)
(.->) = (:->)

-- (.|) :: ADT (PatFn a r) -> ADT (PatFn b r) -> ADT (PatFn (Either a b) r)
(.|) :: ADT (PatFn (Either a b) r) -> ADT (PatFn (Either a b) r) -> ADT (PatFn (Either a b) r)
(.|) = (:|)

pattern NilPat  = InLPat
pattern ConsPat = CompPat InRPat PairPat

adtSum :: [Int] --> Int
adtSum = Rec $ \rec ->
  (NilPat  .-> \(Value ()) -> Value 0) .|
  (ConsPat .-> \(x, xs)    -> Add x (Apply rec xs))

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

-- TODO: Use this type to test things out
data Three a = Three a a a deriving (Generic)
instance ERep a => ERep (Three a)

