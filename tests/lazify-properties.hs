{-# OPTIONS_GHC -Wno-type-defaults #-}
import Data.Lazify
import Data.Monoid
main = do
  print $ case lazify undefined of (_, _) -> "yay"
  print $ lazify (3, 4, 5, 6)
  print $ lazify (Sum (3,4,5))
  print $ case lazify undefined of Sum (Product (_, _, _)) -> "yup!"
