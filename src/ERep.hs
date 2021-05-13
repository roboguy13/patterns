{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module ERep where

import           GHC.Generics

import           Data.Void

class ERep t where
  type ERepTy t
  type ERepTy t = ERepTy (Rep t Void)

  rep :: t -> ERepTy t
  unrep :: ERepTy t -> t

  default rep :: (Generic t, ERep (Rep t Void), ERepTy t ~ ERepTy (Rep t Void)) => t -> ERepTy t
  rep = rep . (from :: t -> Rep t Void)

  default unrep :: (Generic t, ERep (Rep t Void), ERepTy t ~ ERepTy (Rep t Void)) => ERepTy t -> t
  unrep = (to :: Rep t Void -> t) . unrep

type ERepTyIdem a = ERepTy (ERepTy a) ~ ERepTy a
type IsCanonical a = ERepTy a ~ a


instance ERep () where
  type ERepTy () = ()
  rep   = id
  unrep = id

instance ERep (a, b) where
  type ERepTy (a, b) = (a, b)
  rep   = id
  unrep = id

instance ERep (Either a b) where
  type ERepTy (Either a b) = Either a b
  rep   = id
  unrep = id

instance (ERep (f p)) => ERep (M1 i c f p) where
  type ERepTy (M1 i c f p) = ERepTy (f p)

  rep   (M1 x) = rep x
  unrep x      = M1 (unrep x)

instance (ERep (p x), ERep (q x)) => ERep ((p :+: q) x) where
  type ERepTy ((p :+: q) x) = Either (ERepTy (p x)) (ERepTy (q x))

  rep (L1 x) = Left  (rep x)
  rep (R1 y) = Right (rep y)

  unrep (Left  x) = L1 (unrep x)
  unrep (Right y) = R1 (unrep y)

instance (ERep (p x), ERep (q x)) => ERep ((p :*: q) x) where
  type ERepTy ((p :*: q) x) = (ERepTy (p x), ERepTy (q x))

  rep (x :*: y) = (rep x, rep y)
  unrep (x, y) = unrep x :*: unrep y

instance ERep (K1 i c p) where
  type ERepTy (K1 i c p) = c

  rep (K1 x) = x
  unrep = K1

instance ERep (U1 p) where
  type ERepTy (U1 p) = ()

  rep U1 = ()
  unrep () = U1

------
instance ERep Int where
  type ERepTy Int = Int
  rep   = id
  unrep = id

instance ERep a => ERep [a]
instance ERep a => ERep (Maybe a)

