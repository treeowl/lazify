-- | Record types in Haskell can be made lazy through lazy pattern
-- matching. This module offers functions for making them lazy
-- /generically/.
module Data.Lazify (
    Lazifiable (..)
  , genericLazify
  , ($~)
  ) where

import Data.Lazify.Internal
