-- | Record types in Haskell can be made lazy through lazy pattern
-- matching. This module offers functions for making them lazy
-- /generically/. Whereas "Data.Lazify".'Data.Lazify.genericLazify'
-- uses 'Data.Lazify.Lazifiable' to lazify under a @newtype@, the
-- functions in this module expect the underlying type of a @newtype@
-- to be 'GHC.Generics.Generic' as well.
module Data.Lazify.Generic (
    GenericLazifiable (..)
  , ($~)
  ) where
import Data.Lazify.Generic.Internal
