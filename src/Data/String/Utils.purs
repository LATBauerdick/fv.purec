module Data.String.Utils where
-- from https://github.com/cdepillabout/purescript-words-lines/blob/master/src/Data/Array/WordsLines.purs
import Prelude (not, (<<<), map, (==), ($), (<>), (<=), (>=), (||), (&&))

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Char ( toCharCode )

-- | `words` breaks a string up into a list of words, which were delimited
-- | by white space.
-- |
-- | ```purescript
-- | > words ""
-- | [""]
-- | > words "hello"
-- | ["hello"]
-- | > words "hello     "
-- | ["hello"]
-- | > words "   foo     bar   "
-- | ["foo", "bar"]
-- | > words "foo\t\nbar"
-- | ["foo", "bar"]
-- | ```
words :: String -> Array String
words = map fromCharArray <<< go <<< toCharArray
  where
    go :: Array Char -> Array (Array Char)
    go s =
        case Array.uncons $ Array.dropWhile isSpace s of
            Nothing -> []
            Just { head: head, tail: tail } ->
                let withBreaks = break isSpace (head `Array.cons` tail)
                in withBreaks.init `Array.cons` go withBreaks.rest

-- | `break p as` is `span (not <<< p) as`.
break :: forall a . (a -> Boolean) -> Array a -> { init :: (Array a), rest :: (Array a) }
break p = Array.span (not <<< p)

{-- import Data.Char.Unicode (isSpace) --}
-- | Returns `True` for any Unicode space character, and the control
-- | characters `\t`, `\n`, `\r`, `\f`, `\v`.
-- |
-- | `isSpace` includes non-breaking space.
isSpace :: Char -> Boolean
-- The magic 0x377 used in the code below isn't really that magical. As of
-- 2014, all the codepoints at or below 0x377 have been assigned, so we
-- shouldn't have to worry about any new spaces appearing below there.
isSpace c = if uc <= 0x337
               then uc == 32 || (uc >= 9 && uc <= 13) || uc == 0xa0
               else false --???????? uIswspace $ toCharCode -- this is from purescript-unicode
  where
    uc :: Int
    uc = toCharCode c
