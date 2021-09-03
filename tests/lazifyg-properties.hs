{-# OPTIONS_GHC -Wno-type-defaults #-}

import Data.Lazify.Generic
import Data.Monoid
main = do
  print $ case lazifyGeneric undefined of (_, _) -> "yay"
  print $ lazifyGeneric (3, 4, 5, 6)
  print $ case lazifyGeneric undefined of Sum (Product (_, _ ,_)) -> "yep"
