module Data.Ord.Unsafe (
    unsafeCompare
  , unsafeCompareBoolean
  , unsafeCompareInt
  , unsafeCompareNumber
  , unsafeCompareString
  , unsafeCompareChar
  ) where

import Data.Ordering (Ordering(..))

unsafeCompare :: forall a. a -> a -> Ordering
unsafeCompare = unsafeCompareImpl LT EQ GT

foreign import unsafeCompareImpl
  :: forall a
   . Ordering
  -> Ordering
  -> Ordering
  -> a
  -> a
  -> Ordering

unsafeCompareBoolean :: Boolean -> Boolean -> Ordering
unsafeCompareBoolean = ordBooleanImpl LT EQ GT
unsafeCompareInt :: Int -> Int -> Ordering
unsafeCompareInt = ordIntImpl LT EQ GT
unsafeCompareNumber :: Number -> Number -> Ordering
unsafeCompareNumber = ordNumberImpl LT EQ GT
unsafeCompareString :: String -> String -> Ordering
unsafeCompareString = ordStringImpl LT EQ GT
unsafeCompareChar :: Char -> Char -> Ordering
unsafeCompareChar = ordCharImpl LT EQ GT
foreign import ordBooleanImpl :: Ordering -> Ordering -> Ordering -> Boolean -> Boolean -> Ordering
foreign import ordIntImpl     :: Ordering -> Ordering -> Ordering -> Int -> Int -> Ordering
foreign import ordNumberImpl  :: Ordering -> Ordering -> Ordering -> Number -> Number -> Ordering
foreign import ordStringImpl  :: Ordering -> Ordering -> Ordering -> String -> String -> Ordering
foreign import ordCharImpl    :: Ordering -> Ordering -> Ordering -> Char -> Char -> Ordering
