module Data.String.CodePoints
  (
    CodePoint
  , singleton
  , dropWhile
  , fromCodePointArray
  , toCodePointArray
  , codePointFromChar
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.String.Pattern (Pattern(..))
import Data.String.CodeUnits ( fromCharArray, toCharArray, singleton, dropWhile ) as Data.String.CodeUnits
{-- import Data.Int (hexadecimal, toStringAs) --}
{-- import Data.String.Common (toUpper) --}
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), defaultPred, defaultSucc, fromEnum, toEnum, toEnumWithDefaults)

-- | CodePoint is an Int bounded between 0 and 0x10FFFF, corresponding to
-- | Unicode code points.
newtype CodePoint = CodePoint Char

derive instance eqCodePoint :: Eq CodePoint
derive instance ordCodePoint :: Ord CodePoint

instance showCodePoint :: Show CodePoint where
  show (CodePoint i) = "(CodePoint " <> show i <> ")"

-- | Creates a CodePoint from a given Char.
-- |
-- | ```purescript
-- | >>> codePointFromChar 'B'
-- | CodePoint 0x42 -- represents 'B'
-- | ```
-- |
codePointFromChar :: Char -> CodePoint
codePointFromChar = CodePoint

-- | Returns a string of length `1` containing the given character.
singleton :: CodePoint -> String
singleton (CodePoint c) = Data.String.CodeUnits.singleton c

-- | Creates a string from an array of code points. Operates in space and time
-- | linear to the length of the array.
-- |
-- | ```purescript
-- | >>> codePointArray = toCodePointArray "c 𝐀"
-- | >>> codePointArray
-- | [CodePoint 0x63, CodePoint 0x20, CodePoint 0x1D400]
-- | >>> fromCodePointArray codePointArray
-- | "c 𝐀"
-- | ```
-- |
fromCodePointArray :: Array CodePoint -> String
fromCodePointArray cs = Data.String.CodeUnits.fromCharArray (map charFromCodePoint cs) where
  charFromCodePoint (CodePoint c) = c

-- | Creates an array of code points from a string. Operates in space and time
-- | linear to the length of the string.
-- |
-- | ```purescript
-- | >>> codePointArray = toCodePointArray "b 𝐀𝐀"
-- | >>> codePointArray
-- | [CodePoint 0x62, CodePoint 0x20, CodePoint 0x1D400, CodePoint 0x1D400]
-- | >>> map singleton codePointArray
-- | ["b", " ", "𝐀", "𝐀"]
-- | ```
-- |
toCodePointArray :: String -> Array CodePoint
toCodePointArray s = map codePointFromChar (Data.String.CodeUnits.toCharArray s)

-- | Returns the suffix remaining after `takeWhile`.
dropWhile :: (CodePoint -> Boolean) -> String -> String
dropWhile p s = Data.String.CodeUnits.dropWhile pp s where
  pp c= p (CodePoint c)


