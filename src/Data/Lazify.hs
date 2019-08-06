{-# language DataKinds #-}
{-# language DefaultSignatures #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language PolyKinds #-}
{-# language ScopedTypeVariables #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

-- | Record types in Haskell can be made lazy through lazy pattern
-- matching. This module offers functions for making them lazy
-- /generically/.
module Data.Lazify (
    Lazifiable (..)
  , GLazifiable
  , genericLazify
  ) where
import GHC.Generics
import Data.Functor.Product
import Data.Proxy
import Data.Functor.Identity (Identity)
import Data.Functor.Compose (Compose)
import Control.Applicative (Const)

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
  default lazify :: (Generic a, GLazifiable (Rep a)) => a -> a
  lazify x = genericLazify x

class GLazifiable f where
  glazify :: f a -> f a

-- | Lazify a record using its generic representation.
--
-- Note that newtypes are treated specially: a newtype is lazified
-- by lazifying its *underlying* type using its 'Lazifiable' instance.
genericLazify :: (Generic a, GLazifiable (Rep a)) => a -> a
genericLazify = to . glazify . from

-- Non-newtype cases
instance GLazifiable f => GLazifiable (D1 ('MetaData x y z 'False) f) where
  glazify (M1 x) = M1 (glazify x)
instance GLazifiable f => GLazifiable (C1 c f) where
  glazify (M1 x) = M1 (glazify x)
instance GLazifiable f => GLazifiable (S1 c f) where
  glazify (M1 x) = M1 (glazify x)

-- For a newtype, we need to lazify whatever it *wraps*
instance GIsNewtype f => GLazifiable (D1 ('MetaData x y z 'True) f) where
  glazify (M1 x) = M1 (glazifyNewtype x)

instance GLazifiable (K1 i c) where
  glazify x = x

instance GLazifiable U1 where
  glazify _ = U1

instance (GLazifiable f, GLazifiable g) => GLazifiable (f :*: g) where
  glazify ~(x :*: y) = glazify x :*: glazify y

class GIsNewtype f where
  glazifyNewtype :: f a -> f a

instance GIsNewtype f => GIsNewtype (M1 i c f) where
  glazifyNewtype (M1 x) = M1 (glazifyNewtype x)

instance Lazifiable a => GIsNewtype (K1 i a) where
  glazifyNewtype (K1 a) = K1 (lazify a)


-- There is no instance for V1 because an uninhabited datatype can't be
-- lazified.
--
-- There is no instance for f :+: g. A sum can only be lazified if
-- one of its components is *strict* and *uninhabited* while the other
-- is lazifiable. Unfortunately, there are lots of ways this can
-- occur, leading to incompatible constraints.

-- Miscellaneous instances
instance Lazifiable (Proxy a)
instance Lazifiable (Product f g a)
instance Lazifiable a => Lazifiable (Identity a)
instance Lazifiable a => Lazifiable (Const a b)
instance Lazifiable (f (g a)) => Lazifiable (Compose f g a)

-- Tuple instances
instance Lazifiable ()
instance Lazifiable (a,b)
instance Lazifiable (a,b,c)
instance Lazifiable (a,b,c,d)
instance Lazifiable (a,b,c,d,e)
instance Lazifiable (a,b,c,d,e,f)
instance Lazifiable (a,b,c,d,e,f,g)
instance Lazifiable (a,b,c,d,e,f,g,h) where
  lazify ~(a,b,c,d,e,f,g,h) = (a,b,c,d,e,f,g,h)
instance Lazifiable (a,b,c,d,e,f,g,h,i) where
  lazify ~(a,b,c,d,e,f,g,h,i) = (a,b,c,d,e,f,g,h,i)
instance Lazifiable (a,b,c,d,e,f,g,h,i,j) where
  lazify ~(a,b,c,d,e,f,g,h,i,j) = (a,b,c,d,e,f,g,h,i,j)
