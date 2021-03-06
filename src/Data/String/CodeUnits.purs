module Data.String.CodeUnits
  (
    contains
  , singleton
  , indexOf
  , countPrefix
  , drop
  , dropWhile
  , take
  , takeWhile
  , fromCharArray
  , toCharArray
  , length
  ) where

import Prelude

import Data.String.Pattern (Pattern(..))
import Data.Maybe (Maybe(..), isJust)

-- | Returns a string of length `1` containing the given character.
-- |
-- | ```purescript
-- | singleton 'l' == "l"
-- | ```
-- |
foreign import singleton :: Char -> String

-- | Returns the index of the first occurrence of the pattern in the
-- | given string. Returns `Nothing` if there is no match.
-- |
-- | ```purescript
-- | indexOf (Pattern "c") "abcdc" == Just 2
-- | indexOf (Pattern "c") "aaa" == Nothing
-- | ```
-- |
indexOf :: Pattern -> String -> Maybe Int
indexOf = _indexOf Just Nothing

foreign import _indexOf
  :: (forall a. a -> Maybe a)
  -> (forall a. Maybe a)
  -> Pattern
  -> String
  -> Maybe Int

-- | Checks whether the pattern appears in the given string.
-- |
-- | ```purescript
-- | contains (Pattern "needle") "haystack with needle" == true
-- | contains (Pattern "needle") "haystack" == false
-- | ```
contains :: Pattern -> String -> Boolean
contains pat = isJust <<< indexOf pat

foreign import fromCharArray :: Array Char -> String

-- | Converts the string into an array of characters.
-- |
-- | ```purescript
-- | toCharArray "Hello☺\n" == ['H','e','l','l','o','☺','\n']
-- | ```
foreign import toCharArray :: String -> Array Char

-- | Returns the number of code points in the string. Operates in constant
-- | space and in time linear to the length of the string.
-- |
-- | ```purescript
-- | >>> length "b 𝐀𝐀 c 𝐀"
-- | 8
-- | -- compare to Data.String:
-- | >>> length "b 𝐀𝐀 c 𝐀"
-- | 11
-- | ```
-- |
foreign import length :: String -> Int

-- | Returns the number of contiguous characters at the beginning
-- | of the string for which the predicate holds.
-- |
-- | ```purescript
-- | countPrefix (_ /= ' ') "Hello World" == 5 -- since length "Hello" == 5
-- | ```
-- |
foreign import countPrefix :: (Char -> Boolean) -> String -> Int

-- | Returns the string without the first `n` characters.
foreign import drop :: Int -> String -> String

-- | Returns the first `n` characters of the string.
-- |
-- | ```purescript
-- | take 5 "Hello World" == "Hello"
-- | ```
-- |
foreign import take :: Int -> String -> String

-- | Returns the suffix remaining after `takeWhile`.
dropWhile :: (Char -> Boolean) -> String -> String
dropWhile p s = drop (countPrefix p s) s

-- | Returns the longest prefix (possibly empty) of characters that satisfy
-- | the predicate.
-- |
-- | ```purescript
-- | takeWhile (_ /= ':') "http://purescript.org" == "http"
-- | ```
-- |
takeWhile :: (Char -> Boolean) -> String -> String
takeWhile p s = take (countPrefix p s) s

