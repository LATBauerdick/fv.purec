module Data.String
  (
    module Data.String.Pattern
  , length
  , singleton
  , replace
  , fromCharArray
  , toCharArray
  , drop
  , dropWhile
  , countPrefix
  ) where

import Data.String.Pattern (Pattern(..), Replacement(..))

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

foreign import fromCharArray :: Array Char -> String

-- | Converts the string into an array of characters.
-- |
-- | ```purescript
-- | toCharArray "Hello☺\n" == ['H','e','l','l','o','☺','\n']
-- | ```
foreign import toCharArray :: String -> Array Char

-- | Returns the suffix remaining after `takeWhile`.
dropWhile :: (Char -> Boolean) -> String -> String
dropWhile p s = drop (countPrefix p s) s

-- | Returns a string of length `1` containing the given character.
foreign import singleton :: Char -> String

-- | Replaces the first occurence of the first argument with the second argument.
foreign import replace :: Pattern -> Replacement -> String -> String

-- | Returns the string without the first `n` characters.
foreign import drop :: Int -> String -> String

-- | Returns the number of contiguous characters at the beginning
-- | of the string for which the predicate holds.
-- |
-- | ```purescript
-- | countPrefix (_ /= ' ') "Hello World" == 5 -- since length "Hello" == 5
-- | ```
-- |
foreign import countPrefix :: (Char -> Boolean) -> String -> Int

