{-# language AllowAmbiguousTypes #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language PolyKinds #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}
{-# language TypeInType #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
{-# language TypeApplications #-}

{-# OPTIONS_HADDOCK not-home #-}

-- | Record types in Haskell can be made lazy through lazy pattern
-- matching. This module offers functions for making them lazy
-- /generically/.
module Data.Lazify.Internal (
    Lazifiable (..)
  , GLazifiable (..)
  , genericLazify
  , ($~)
  ) where
import GHC.Generics
import Data.Functor.Product
import Data.Proxy
import Data.Functor.Identity (Identity)
import Control.Monad.Trans.Identity (IdentityT)
import Data.Functor.Compose (Compose)
import Data.Coerce (Coercible)
import Data.Type.Coercion (Coercion(..))
import Control.Applicative (Const)
import Data.Tagged (Tagged)
import GHC.Exts (TYPE)
import Data.Type.Equality ((:~:)(..))
import qualified Data.Monoid as M
import qualified Data.Semigroup as S
import Data.List.NonEmpty (NonEmpty)
import Data.Type.Equality ((:~~:)(..), type (~~))
import Type.Reflection (Typeable, TypeRep, typeRep)
import Data.Tree (Tree (..))
#if MIN_VERSION_base (4,15,0)
import GHC.Tuple (Solo)
#endif
import qualified Control.Applicative.Backwards as AppBackwards
import qualified Data.Functor.Reverse as TravReverse
import GHC.TypeLits

-- | A class for types that can be lazified. A generic
-- default is provided for convenience. To lazify a type using
-- its generic representation, use 'genericLazify'.
class Lazifiable a where
  -- | Lazily rewrap a record. Applying @lazify@ to a record and then
  -- pattern matching on it strictly is equivalent to pattern matching
  -- on it lazily.
  --
  -- @
  -- strictFirst :: (a -> a') -> (a, b) -> (a', b)
  -- strictFirst f (a, b) = (f a, b)
  --
  -- lazyFirst :: (a -> a') -> (a, b) -> (a', b)
  -- lazyFirst f = strictFirst f . lazify
  -- -- Equivalently
  -- lazyFirst f ~(a, b) = (f a, b)
  -- @
  lazify :: a -> a
  default lazify :: (Generic a, GLazifiable a (Rep a)) => a -> a
  lazify x = genericLazify x

-- | A 'Generic' representation that can be lazified.
class GLazifiable a f where
  -- | Lazify a 'Generic' representation.
  glazify :: f p -> f p

-- | Lazify a record using its generic representation.
--
-- Note that newtypes are treated specially: a newtype is lazified
-- by lazifying its /underlying/ type using its 'Lazifiable' instance.
genericLazify :: forall a. (Generic a, GLazifiable a (Rep a)) => a -> a
genericLazify = to . glazify @a . from

-- | Apply a function to a lazified value.
--
-- Note to users of @TypeApplications@: For GHC >= 9.0.1, the representation
-- is marked as inferred. Before that, doing so is impossible and the
-- representation must be passed as the first type argument. I'm sorry.
#if __GLASGOW_HASKELL__ >= 900
($~) :: forall {rep} a (b :: TYPE rep). Lazifiable a => (a -> b) -> a -> b
#else
($~) :: forall rep a (b :: TYPE rep). Lazifiable a => (a -> b) -> a -> b
#endif
f $~ a = f (lazify a)

-- Non-newtype cases
instance GLazifiable a f => GLazifiable a (D1 ('MetaData x y z 'False) f) where
  glazify (M1 x) = M1 (glazify @a x)
instance GLazifiable a f => GLazifiable a (C1 c f) where
  glazify (M1 x) = M1 (glazify @a x)

instance GLazifiable a f => GLazifiable a (S1 ('MetaSel _p _q _r 'DecidedLazy) f) where
  glazify (M1 m) = M1 (glazify @a m)
instance TypeError ('Text "Can't lazify " ':<>: 'ShowType a ':<>: 'Text ":"
                    ':$$: 'Text "It has a strict field.")
  => GLazifiable a (S1 ('MetaSel _p _q _r 'DecidedStrict) f) where
  glazify _ = error "Unreachable"
instance TypeError ('Text "Can't lazify " ':<>: 'ShowType a ':<>: 'Text ":"
                    ':$$: 'Text "It has a strict (unpacked) field.")
  => GLazifiable a (S1 ('MetaSel _p _q _r 'DecidedUnpack) f) where
  glazify _ = error "Unreachable"

-- For a newtype, we need to lazify whatever it *wraps*
instance Lazifiable c
    => GLazifiable a (D1 ('MetaData x y z 'True) (C1 _m (S1 _o (Rec0 c)))) where
  glazify (M1 (M1 (M1 (K1 x)))) = M1 (M1 (M1 (K1 (lazify x))))

instance GLazifiable a (K1 i c) where
  glazify x = x

instance GLazifiable a U1 where
  glazify _ = U1

instance (GLazifiable a f, GLazifiable a g) => GLazifiable a (f :*: g) where
  glazify ~(x :*: y) = glazify @a x :*: glazify @a y

-- There is no instance for V1 because an uninhabited datatype can't be
-- lazified.
--
-- There is no instance for f :+: g. A sum can only be lazified if
-- one of its components is *strict* and *uninhabited* while the other
-- is lazifiable. Unfortunately, there are lots of ways this can
-- occur, leading to incompatible constraints.
instance TypeError ('Text "Can't lazify " ':<>: 'ShowType a ':<>: 'Text ":"
                    ':$$: 'Text "It is a sum type.")
  => GLazifiable a (f :+: g) where
  glazify _ = error "Unreachable"

-- Miscellaneous instances
instance Lazifiable (Proxy a)
instance Lazifiable (Product f g a)
instance Lazifiable a => Lazifiable (Identity a)
instance Lazifiable a => Lazifiable (Const a b)
instance Lazifiable b => Lazifiable (Tagged a b)
instance Lazifiable (f (g a)) => Lazifiable (Compose f g a)
instance Lazifiable (f (g a)) => Lazifiable ((f :.: g) a)

instance Lazifiable a => Lazifiable (S.First a)
instance Lazifiable a => Lazifiable (S.Last a)
instance Lazifiable a => Lazifiable (S.Min a)
instance Lazifiable a => Lazifiable (S.Max a)
instance Lazifiable a => Lazifiable (S.Product a)
instance Lazifiable a => Lazifiable (S.Sum a)
instance Lazifiable a => Lazifiable (S.Dual a)
instance Lazifiable a => Lazifiable (S.WrappedMonoid a)
instance Lazifiable (S.Arg a b)
instance Lazifiable (NonEmpty a)
instance Lazifiable (Tree a)

instance Lazifiable (f a) => Lazifiable (M.Alt f a)
#if MIN_VERSION_base(4,12,0)
instance Lazifiable (f a) => Lazifiable (M.Ap f a)
#endif
instance Lazifiable (f a) => Lazifiable (AppBackwards.Backwards f a)
instance Lazifiable (t a) => Lazifiable (TravReverse.Reverse t a)
instance Lazifiable (f a) => Lazifiable (IdentityT f a)

-- Singletons are generally lazifiable under sufficiently boring
-- conditions. These could, at least theoretically, help guide type
-- inference in some cases, if it's more convenient to explain
-- how one *could* get the singleton than to pin down its type
-- by hand.
instance a ~ b => Lazifiable (a :~: b) where
  lazify _ = Refl

instance a ~~ b => Lazifiable (a :~~: b) where
  lazify _ = HRefl

instance Typeable a => Lazifiable (TypeRep a) where
  lazify _ = typeRep

instance Coercible a b => Lazifiable (Coercion a b) where
  lazify _ = Coercion

-- Tuple instances
instance Lazifiable ()
#if MIN_VERSION_base (4,15,0)
instance Lazifiable (Solo a)
#endif
instance Lazifiable (a,b)
instance Lazifiable (a,b,c)
instance Lazifiable (a,b,c,d)
instance Lazifiable (a,b,c,d,e)
instance Lazifiable (a,b,c,d,e,f)
instance Lazifiable (a,b,c,d,e,f,g)
-- The below are written by hand because the generic
-- versions are too big for GHC to compile away the
-- cruft.
instance Lazifiable (a,b,c,d,e,f,g,h) where
  lazify ~(a,b,c,d,e,f,g,h) = (a,b,c,d,e,f,g,h)
instance Lazifiable (a,b,c,d,e,f,g,h,i) where
  lazify ~(a,b,c,d,e,f,g,h,i) = (a,b,c,d,e,f,g,h,i)
instance Lazifiable (a,b,c,d,e,f,g,h,i,j) where
  lazify ~(a,b,c,d,e,f,g,h,i,j) = (a,b,c,d,e,f,g,h,i,j)
