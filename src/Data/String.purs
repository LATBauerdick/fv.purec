module Data.String
  ( length
  , fromCharArray
  , toCharArray
  ) where

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
{-- fromCharArray :: Array Char -> String --}
{-- fromCharArray s = showCharImpl $ fromMaybe 'x' $ A.head s --}
foreign import fromCharArrayImpl :: Array Char -> String
fromCharArray :: Array Char -> String
fromCharArray = fromCharArrayImpl

-- | Converts the string into an array of characters.
-- |
-- | ```purescript
-- | toCharArray "Hello☺\n" == ['H','e','l','l','o','☺','\n']
-- | ```
foreign import toCharArray :: String -> Array Char

foreign import lengthImpl :: String -> Int
length :: String -> Int
length = lengthImpl --???????????Array.length <<< toCodePointArray
