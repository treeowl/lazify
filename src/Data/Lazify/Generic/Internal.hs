{-# language AllowAmbiguousTypes #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeInType #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

{-# OPTIONS_HADDOCK not-home #-}

-- | Record types in Haskell can be made lazy through lazy pattern
-- matching. This module offers functions for making them lazy
-- /generically/.
module Data.Lazify.Generic.Internal (
    GenericLazifiable (..)
  , LazifiableG (..)
  , ($~)
  ) where
import GHC.Generics
import GHC.Exts (TYPE)
import GHC.TypeLits
import Data.Type.Equality ((:~:)(..), (:~~:)(..), type (~~))
import Data.Type.Coercion (Coercion (..))
import Data.Coerce
import Type.Reflection (Typeable, TypeRep, typeRep)

-- | This class is intended to be used primarily with the
-- generic instance given in this module. However, users are free
-- to write @{-\# OVERLAPPING \#-}@ instances whenever necessary.
class GenericLazifiable a where
  -- | Lazify a record using its generic representation.
  --
  -- Note that newtypes are treated specially: a newtype is lazified
  -- by lazifying its /underlying/ type.
  lazifyGeneric :: a -> a

-- | The generic instance.
instance (Generic a, LazifiableG a (Rep a)) => GenericLazifiable a where
  lazifyGeneric = to . lazifyG @a . from

-- Instances for non-Generic types
instance {-# OVERLAPPING #-} a ~ b => GenericLazifiable (a :~: b) where
  lazifyGeneric _ = Refl

instance {-# OVERLAPPING #-} a ~~ b => GenericLazifiable (a :~~: b) where
  lazifyGeneric _ = HRefl

instance {-# OVERLAPPING #-} Typeable a => GenericLazifiable (TypeRep a) where
  lazifyGeneric _ = typeRep

instance {-# OVERLAPPING #-} Coercible a b => GenericLazifiable (Coercion a b) where
  lazifyGeneric _ = Coercion

-- Instances for large tuples whose generic instances
-- are too big to optimize well.
instance {-# OVERLAPPING #-} GenericLazifiable (a,b,c,d,e,f,g,h) where
  lazifyGeneric ~(a,b,c,d,e,f,g,h) = (a,b,c,d,e,f,g,h)
instance {-# OVERLAPPING #-} GenericLazifiable (a,b,c,d,e,f,g,h,i) where
  lazifyGeneric ~(a,b,c,d,e,f,g,h,i) = (a,b,c,d,e,f,g,h,i)
instance {-# OVERLAPPING #-} GenericLazifiable (a,b,c,d,e,f,g,h,i,j) where
  lazifyGeneric ~(a,b,c,d,e,f,g,h,i,j) = (a,b,c,d,e,f,g,h,i,j)

-- | A 'Generic' representation that can be lazified.
class LazifiableG a f where
  -- | Lazify a 'Generic' representation.
  lazifyG :: f p -> f p

-- | Apply a function to a lazified value.
--
-- Note to users of @TypeApplications@: For GHC >= 9.0.1, the representation
-- is marked as inferred. Before that, doing so is impossible and the
-- representation must be passed as the first type argument. I'm sorry.
#if __GLASGOW_HASKELL__ >= 900
($~) :: forall {rep} a (b :: TYPE rep). GenericLazifiable a => (a -> b) -> a -> b
#else
($~) :: forall rep a (b :: TYPE rep). GenericLazifiable a => (a -> b) -> a -> b
#endif
f $~ a = f (lazifyGeneric a)

-- Non-newtype cases
instance LazifiableG a f => LazifiableG a (D1 ('MetaData x y z 'False) f) where
  lazifyG (M1 x) = M1 (lazifyG @a x)
instance LazifiableG a f => LazifiableG a (C1 c f) where
  lazifyG (M1 x) = M1 (lazifyG @a x)
instance LazifiableG a f => LazifiableG a (S1 ('MetaSel _p _q _r 'DecidedLazy) f) where
  lazifyG (M1 m) = M1 (lazifyG @a m)
instance TypeError ('Text "Can't lazify " ':<>: 'ShowType a ':<>: 'Text ":"
                    ':$$: 'Text "It has a strict field.")
  => LazifiableG a (S1 ('MetaSel _p _q _r 'DecidedStrict) f) where
  lazifyG _ = error "Unreachable"
instance TypeError ('Text "Can't lazify " ':<>: 'ShowType a ':<>: 'Text ":"
                    ':$$: 'Text "It has a strict (unpacked) field.")
  => LazifiableG a (S1 ('MetaSel _p _q _r 'DecidedUnpack) f) where
  lazifyG _ = error "Unreachable"

-- For a newtype, we need to lazify whatever it *wraps*.
-- Unfortunately, we lose error context here, but we want
-- users to be able to write overlapping instances of GenericLazifiable
-- if they really need to.
instance GenericLazifiable c => LazifiableG a (D1 ('MetaData x y z 'True) (C1 _p (S1 _q (Rec0 c)))) where
  lazifyG (M1 (M1 (M1 (K1 x)))) = M1 (M1 (M1 (K1 (lazifyGeneric x))))

instance LazifiableG a (K1 i c) where
  lazifyG x = x

instance LazifiableG a U1 where
  lazifyG _ = U1

instance (LazifiableG a f, LazifiableG a g) => LazifiableG a (f :*: g) where
  lazifyG ~(x :*: y) = lazifyG @a x :*: lazifyG @a y

-- There is no instance for V1 because an uninhabited datatype can't be
-- lazified.

-- There is no instance for f :+: g. A sum can only be lazified if
-- one of its components is *strict* and *uninhabited* while the other
-- is lazifiable. Unfortunately, there are lots of ways this can
-- occur, leading to incompatible constraints.
instance TypeError ('Text "Can't lazify " ':<>: 'ShowType a ':<>: 'Text ":"
                    ':$$: 'Text "It is a sum type.")
  => LazifiableG a (f :+: g) where
  lazifyG _ = error "Unreachable"
