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
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}

import           Control.Applicative
import           Data.Kind

import           GHC.Generics

import           ERep
import           Pattern

import           Data.Constraint

-- TODO: Add a type class that keeps track of constructors and eliminators

data Match f a b where
  -- | Similar to Mark Tullsen's First Class Patterns paper
  (:->) ::
    Pattern f (f s) t -> (t -> f r) -> Match f s r

  (:|) ::
    Match f (Either a b) r -> Match f (Either a b) r -> Match f (Either a b) r


data E t where
  Lit :: Int -> E Int
  Add :: E Int -> E Int -> E Int

  MatchE :: E a -> Match E (ERepTy a) b -> E b

  Apply :: E (a -> b) -> E a -> E b
  Lam :: Value E a => (E a -> E b) -> E (a -> b)

  Nil :: E [a]
  Cons :: E a -> E [a] -> E [a]

  Rec :: (E (a -> b) -> E (a -> b)) -> E (a -> b)

pattern NilPat  = CompPat InLPat BasePat
pattern ConsPat = CompPat InRPat PairPat

class Value f a where
  value :: a -> f a

instance Value E Int where
  value = Lit

instance Value E a => Value E [a] where
  value [] = Nil
  value (x:xs) = Cons (value x) $ value xs

class Matchable f where
    -- TODO: See if this list can be reasonably put into this typeclass.
    -- This could be useful, since the collection of possible "simple" pattern of
    -- a type is finite, so they can be enumerated. This could be useful in
    -- some situations.
  -- patterns :: [SomePattern f (f a)]

  fromPattern :: Pattern f (f (ERepTy s)) t -> f s -> Maybe t

-- TODO: See if Template Haskell can auto-generate these instances for
-- a given list of constructors (in this case, Nil and Cons)
instance Matchable E  where
  -- patterns = [SomePattern NilPat, SomePattern ConsPat]

  fromPattern NilPat Nil = Just ()
  fromPattern ConsPat (Cons x xs) = Just (x, xs)
  fromPattern _ _ = Nothing


matchPattern :: Matchable f => Pattern f (f (ERepTy s)) t -> (t -> f r) -> f s -> Maybe (f r)
matchPattern pat f arg = f <$> fromPattern pat arg

match :: Matchable f => Match f (ERepTy a) b -> f a -> Maybe (f b)
match (pat :-> f) arg = matchPattern pat f arg
match (p :| q)    arg = match p arg <|> match q arg

matchWith :: Matchable f => f a -> Match f (ERepTy a) b -> Maybe (f b)
matchWith = flip match

adtSum :: E ([Int] -> Int)
adtSum = Rec $ \rec ->
  Lam $ \x ->
    MatchE x $
      (NilPat  :-> \()      -> Lit 0) :|
      (ConsPat :-> \(x, xs) -> Add x (Apply rec xs))

listToInt :: E ([Int] -> Int)
listToInt =
  Lam $ \x ->
    MatchE x $
      (NilPat  :-> \()    -> Lit 0) :|
      (ConsPat :-> \(_,_) -> Lit 1)

eval :: E t -> t
eval (Lit x)      = x
eval (Add x y)    = eval x + eval y
eval (Lam f)      = eval . f . value
eval (Apply f x)  = eval f (eval x)
eval Nil          = []
eval (Cons x xs)  = eval x : eval xs
eval (MatchE x m) =
  case match m x of
    Nothing -> error "eval: Non-exhaustive match in EDSL code"
    Just r -> eval r
eval rec@(Rec f)  = eval $ f rec

