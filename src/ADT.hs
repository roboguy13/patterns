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
  -- Rec :: ERepTy a ~ a => ((ERepTy a -|E|-> b) -> (ERepTy a -|E|-> b)) -> E (a --> b)

pattern NilPat  = CompPat InLPat BasePat
pattern ConsPat = CompPat InRPat PairPat

type x --> y = PatFn x y

newtype PatFnF f x y = PatFnF { runPatFunF :: f (PatFn x y) }

class Value f a where
  value :: a -> f a

instance Value E Int where
  value = Lit

instance Value E a => Value E [a] where
  value [] = Nil
  value (x:xs) = Cons (value x) $ value xs

-- Similar to "ternary" type-level operators used by Edward Kmett and
-- Iceland Jack
infixl 3 -|, |->
type x -|  f = PatFnF f x
type f |-> y = f y

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

  -- match (NilPat  :-> f) Nil = Just (f ())
  -- match (ConsPat :-> f) (Cons x xs) = Just (f (x, xs))
  -- match (p :| q) arg = match p arg <|> match q arg
  -- match _ _ = Nothing


matchPattern :: Matchable f => Pattern f (f (ERepTy s)) t -> (t -> f r) -> f s -> Maybe (f r)
matchPattern pat f arg = f <$> fromPattern pat arg

match :: Matchable f => Match f (ERepTy a) b -> f a -> Maybe (f b)
match (pat :-> f) arg = matchPattern pat f arg
match (p :| q)    arg = match p arg <|> match q arg

-- match' :: Match f (ERepTy a) b -> f a -> Maybe (f b)
-- match' = undefined

matchWith :: Matchable f => f a -> Match f (ERepTy a) b -> Maybe (f b)
matchWith = flip match

adtSum :: E ([Int] -> Int)
adtSum = Rec $ \rec ->
  Lam $ \x ->
    MatchE x $
      (NilPat  :-> \()      -> Lit 0) :|
      (ConsPat :-> \(x, xs) -> Add x (Apply rec xs))

eval :: E t -> t
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Lam f) = eval . f . value
eval (Apply f x) = eval f (eval x)
eval Nil = []
eval (Cons x xs) = eval x : eval xs
eval (MatchE x m) =
  case match m x of
    Nothing -> error "eval: Non-exhaustive match in EDSL code"
    Just r -> eval r
eval rec@(Rec f) = eval $ f rec

-- fromPattern :: ERepTyIdem s => Pattern f (f (ERepTy s)) t -> f s -> Maybe t
-- -- fromPattern BasePat x = _
-- fromPattern PairPat (rep -> (x, y)) = undefined


-- fromPattern BasePat arg@(Base x) = Just x
-- fromPattern PairPat (Pair' x y) = Just (x, y)
-- fromPattern InLPat  (InL' x) = Just x
-- fromPattern InRPat  (InR' y) = Just y
-- fromPattern (CompPat p q) arg = do
--   x <- fromPattern p arg
--   fromPattern q x
-- fromPattern _ _ = Nothing

-- fromPattern :: Matchable f a => Pattern f (f (ERepTy a)) (f b) -> f a -> Maybe (f b)
-- fromPattern p = match (p :-> id)

-- x -|f|-> y  =  (x -| f) |-> y
--                (f (PatFn x)

-- TODO: Look into using Template Haskell to automatically generate pattern
-- synonyms like these
-- pattern NilPat :: Pattern f (f (Either t b2)) t
-- pattern NilPat :: Pattern E (E s) t1

-- pattern NilPat :: Pattern f (f (Either t b2)) t

class ERep t => DSL f t where
  toDSL :: ERepTy t -> f t

  -- dslEmbed :: f t -> ADT f (ERepTy t)

  -- default dslEmbed :: ERepTy t ~ t => f t -> ADT f (ERepTy t)
  -- dslEmbed = Base




{-
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

instance {-# OVERLAPS #-} (DSL f a) => DSL (ADT f) a where
  toDSL = Base . toDSL
  -- dslEmbed = undefined --Base . _

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

  -- dslEmbed Nil = InL' Unit'
  -- dslEmbed (Cons x xs) = InR' (Pair' (Base x) (Base xs))

adtSum :: ERepTy [Int] --> Int
adtSum = Rec $ \rec ->
  (NilPat  :-> \()      -> Lit 0) :|
  (ConsPat :-> \(x, xs) -> Add x (Apply rec xs))

listToInt :: ERepTy [Int] --> Int
listToInt =
  (NilPat  :-> \()    -> Lit 0) :|
  (ConsPat :-> \(_,_) -> Lit 1)

fromPattern :: Pattern ADT (ADT f) (ADT f s) t -> ADT f s -> Maybe t
fromPattern BasePat arg@(Base x) = Just x
fromPattern PairPat (Pair' x y) = Just (x, y)
fromPattern InLPat  (InL' x) = Just x
fromPattern InRPat  (InR' y) = Just y
fromPattern (CompPat p q) arg = do
  x <- fromPattern p arg
  fromPattern q x
fromPattern _ _ = Nothing

-- matchPattern :: Pattern ADT (ADT s) t -> (t -> ADT r) -> ADT s -> Maybe (ADT r)
-- matchPattern pat f arg = f <$> fromPattern pat arg
{-
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

-}

