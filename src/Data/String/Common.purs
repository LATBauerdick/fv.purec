module Data.String.Common
  ( null
  , replace
  ) where

import Prelude

import Data.String.Pattern (Pattern, Replacement)

-- | Returns `true` if the given string is empty.
-- |
-- | ```purescript
-- | null "" == true
-- | null "Hi" == false
-- | ```
null :: String -> Boolean
null s = s == ""

-- | Replaces the first occurence of the pattern with the replacement string.
-- |
-- | ```purescript
-- | replace (Pattern "<=") (Replacement "≤") "a <= b <= c" == "a ≤ b <= c"
-- | ```
foreign import replace :: Pattern -> Replacement -> String -> String
