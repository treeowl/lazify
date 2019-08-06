{-# language CPP #-}
{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language PolyKinds #-}
{-# language ScopedTypeVariables #-}
{-# language TypeInType #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

{-# OPTIONS_HADDOCK not-home #-}

-- | Record types in Haskell can be made lazy through lazy pattern
-- matching. This module offers functions for making them lazy
-- /generically/.
module Data.Lazify.Generic.Internal (
    LazifiableG (..)
  , IsNewtypeG (..)
  , lazifyGeneric
  , ($~)
  ) where
import GHC.Generics
import GHC.Exts (TYPE)

-- | A 'Generic' representation that can be lazified.
class LazifiableG f where
  -- | Lazify a 'Generic' representation.
  lazifyG :: f a -> f a

-- | Lazify a record using its generic representation.
--
-- Note that newtypes are treated specially: a newtype is lazified
-- by lazifying its /underlying/ type.
lazifyGeneric :: (Generic a, LazifiableG (Rep a)) => a -> a
lazifyGeneric = to . lazifyG . from

-- | Apply a function to a lazified value.
($~) :: forall rep a (b :: TYPE rep). (Generic a, LazifiableG (Rep a)) => (a -> b) -> a -> b
f $~ a = f (lazifyGeneric a)

-- Non-newtype cases
instance LazifiableG f => LazifiableG (D1 ('MetaData x y z 'False) f) where
  lazifyG (M1 x) = M1 (lazifyG x)
instance LazifiableG f => LazifiableG (C1 c f) where
  lazifyG (M1 x) = M1 (lazifyG x)
instance LazifiableG f => LazifiableG (S1 c f) where
  lazifyG (M1 x) = M1 (lazifyG x)

-- For a newtype, we need to lazify whatever it *wraps*
instance IsNewtypeG f => LazifiableG (D1 ('MetaData x y z 'True) f) where
  lazifyG (M1 x) = M1 (lazifyNewtypeG x)

instance LazifiableG (K1 i c) where
  lazifyG x = x

instance LazifiableG U1 where
  lazifyG _ = U1

instance (LazifiableG f, LazifiableG g) => LazifiableG (f :*: g) where
  lazifyG ~(x :*: y) = lazifyG x :*: lazifyG y

-- | A 'Generic' representation that should be lazified @newtype@-style.
-- That is, its /contents/ should be lazified.
class IsNewtypeG f where
  -- | Dig into a @newtype@ and lazify its payload generically.
  lazifyNewtypeG :: f a -> f a

instance IsNewtypeG f => IsNewtypeG (M1 i c f) where
  lazifyNewtypeG (M1 x) = M1 (lazifyNewtypeG x)

instance (Generic a, LazifiableG (Rep a)) => IsNewtypeG (K1 i a) where
  lazifyNewtypeG (K1 a) = K1 (to . lazifyG . from $ a)

-- There is no instance for V1 because an uninhabited datatype can't be
-- lazified.
--
-- There is no instance for f :+: g. A sum can only be lazified if
-- one of its components is *strict* and *uninhabited* while the other
-- is lazifiable. Unfortunately, there are lots of ways this can
-- occur, leading to incompatible constraints.
