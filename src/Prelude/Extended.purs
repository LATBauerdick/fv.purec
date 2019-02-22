module Prelude.Extended (
    module Prelude
  , List (..), range, fromList
  , fromIntegral
  , words, unwords, unlines
  , sqr, mod'
  , irem, iflt
  , prettyMatrix
  , normals, stats
  , debug, trace
  , uidx, uJust
  , to1fix, to2fix, to3fix, to5fix
  , error
  , undefined
  , boxMuller
  ) where

import Prelude
import Effect ( Effect )
import Effect.Console ( log )
import Effect.Random ( random )
import Math ( log, sqrt, pi, sin, cos ) as Math
import Data.Int ( round, toNumber, floor )
import Partial.Unsafe (unsafePartial, unsafeCrashWith)
import Unsafe.Coerce ( unsafeCoerce ) as Unsafe.Coerce
import Data.Foldable ( class Foldable, foldr, sum, maximum )
import Data.List ( List(..), (:), range, fromFoldable ) as L
import Data.Array ( unsafeIndex, range, length, take, concat
  , fromFoldable, replicate
  ) as A
import Data.Enum (fromEnum)
import Data.Unfoldable ( replicate )
import Data.Traversable ( sequence )
import Data.Tuple ( Tuple(..), fst, snd )
import Data.Maybe ( Maybe(..), fromMaybe', fromMaybe, fromJust )
import Data.String.CodeUnits ( fromCharArray )
import Data.String.CodeUnits ( length, dropWhile ) as S
import Data.String.CodeUnits ( takeWhile )
import Text.Format ( format, precision, width )
import Control.MonadZero ( guard )
import Effect.Unsafe (unsafePerformEffect)

-- List, PureScript does not provide sugar
type List a = L.List a
range :: Int -> Int -> List Int
range f t = L.range f t
fromList :: forall f. Foldable f => f ~> Array
fromList = A.fromFoldable

fromIntegral :: Int -> Number
fromIntegral = toNumber

trace :: forall a. String -> a -> a
trace s a = const a (unsafePerformEffect (log s))
{-- trace s a = const a (unsafeLog s) --}
{-- foreign import unsafeLog :: String -> String --}

debug :: forall a. a -> String -> a
debug = flip trace

-- | square of a number
sqr :: Number -> Number
sqr a = a*a

-- | generalisation of 'div' to any instance of Real
div' :: Number -> Number -> Int
div' n d = floor ( n /  d)

-- | generalisation of 'divMod' to any instance of Real
divMod' :: Number -> Number -> (Tuple Int Number)
divMod' n d = (Tuple f (n - (toNumber f) * d)) where
    f = div' n d

-- | generalisation of 'mod' to any instance of Real
mod' :: Number -> Number -> Number
mod' n d = n - (toNumber f) * d where
    f = div' n d

-- | unsafe index to Array
uidx :: forall a. Array a -> Int -> a
uidx = unsafePartial A.unsafeIndex

uJust :: forall a. Maybe a -> a
uJust = unsafePartial $ fromJust

prettyMatrix :: Int -> Int -> Array Number -> String
prettyMatrix r c v = unlines ls where
-- | /O(1)/. Unsafe variant of 'getElem', without bounds checking.
  unsafeGet :: Int          -- ^ Row
            -> Int          -- ^ Column
            -> Array Number -- ^ Matrix
            -> Number
  unsafeGet i j vv = unsafePartial $ A.unsafeIndex vv $ encode c i j
  encode :: Int -> Int -> Int -> Int
  encode m i j = (i-1)*m + j - 1
  ls = do
    i <- range 1 r
    let ws :: List String
        ws = map (\j -> fillBlanks mx (to3fix $ unsafeGet i j v)) (range 1 c)
    pure $ "( " <> unwords ws <> " )"
  mx = fromMaybe 0 (maximum $ map (S.length <<< to3fix) v)
  fillBlanks k str =
    (fromCharArray $ A.replicate (k - S.length str) ' ') <> str

-- | filter list of objects given list of indices in [a]
-- | return list with only those b that have  indices that  are in rng [a]
iflt :: forall a. Array Int -> Array a  -> Array a
iflt rng hl = do
  i <- rng
  pure $ uidx hl i

-- | remove element at index
irem :: forall a. Int -> Array a -> Array a
irem indx hl = do
  i <- A.range 0 ((A.length hl)-1)
  guard $ i /= indx
  pure $ uidx hl i

-- | round to 3 decimal
roundDec :: Number -> Number
roundDec x = (toNumber (round ( 1000.0 * x )))/1000.0

to0fix :: Number -> String
to0fix = format (width 4 <> precision 0)
to1fix :: Number -> String
to1fix = format (width 6 <> precision 1)
to2fix :: Number -> String
to2fix = format (width 7 <> precision 2)
to3fix :: Number -> String
to3fix = format (width 8 <> precision 3)
to5fix :: Number -> String
to5fix = format (width 10 <> precision 5)

-- | 'words' breaks a string up into a list of words, which were delimited
-- | by white space.
isSpace :: Char -> Boolean
{-- isSpace c = c == ' ' || c == '\n' --}
isSpace c = uc == 32 || (uc >= 9 && uc <= 13) || uc == 0xa0
  where
        uc :: Int
        uc = toCharCode c

-- | Returns the numeric Unicode value of the character.
{-- foreign import toCharCode :: Char -> Int --}
toCharCode :: Char -> Int
toCharCode = fromEnum

words :: String -> List String
{-- words = L.fromFoldable <<< Data.String.Utils.words --}
words s = case S.dropWhile isSpace s of
                                "" -> L.Nil
                                str' -> let s0 = takeWhile (not isSpace) str'
                                            s1 = S.dropWhile (not isSpace) str'
                                        in s0 L.: words s1

-- | 'break', applied to a predicate @p@ and a list @xs@, returns a tuple where
-- | first element is longest prefix (possibly empty) of @xs@ of elements that
-- | /do not satisfy/ @p@ and second element is the remainder of the list:
--
-- > break (> 3) [1,2,3,4,1,2,3,4] == ([1,2,3],[4,1,2,3,4])
-- > break (< 9) [1,2,3] == ([],[1,2,3])
-- > break (> 9) [1,2,3] == ([1,2,3],[])
--
-- 'break' @p@ is equivalent to @'span' ('not' . p)@.

break :: forall a. (a -> Boolean) -> List a -> (Tuple (List a) (List a))
-- HBC version (stolen)
break _ L.Nil             =  (Tuple L.Nil L.Nil)
break p xs@(x L.: xs')
           | p x        =  (Tuple L.Nil xs)
           | otherwise  =  let yszs = break p xs'
                               ys = fst yszs
                               zs = snd yszs
                           in (Tuple (x L.: ys) zs)

-- | 'unwords' is an inverse operation to 'words'.
-- It joins words with separating spaces.
unwords                 :: List String -> String
unwords L.Nil             =  ""
unwords ws              =  foldr1 (\w s -> w <> " " <> s) ws

-- | 'unlines' is an inverse operation to 'lines'.
-- It joins lines, after appending a terminating newline to each.
unlines                 :: List String -> String
unlines L.Nil = ""
unlines (l L.: ls) = l <> "\n" <> unlines ls

-- | A variant of 'foldr' that has no base case,
-- and thus may only be applied to non-empty structures.
--
-- @'foldr1' f = 'List.foldr1' f . 'toList'@
foldr1 :: forall a t. Foldable t => Show a => (a -> a -> a) -> t a -> a
foldr1 f xs = fromMaybe' ( \_ -> error $ "foldr1: empty structure" <> show xx) xx
                         where
  xx = (foldr mf Nothing xs)
  mf :: a -> Maybe a -> Maybe a
  mf acc m = Just (case m of
                         Nothing -> acc
                         Just y  -> f acc y)

undefined :: forall a. a
undefined = Unsafe.Coerce.unsafeCoerce unit

error :: forall a. String -> a
error = unsafeCrashWith

-- | generate a list of n normally distributed random values
-- | usinging the Box-Muller method and the random function
boxMuller :: Effect (Array Number)
boxMuller = do
              u1 <- random
              u2 <- random
              let r = Math.sqrt (-2.0 * Math.log u1)
                  t = 2.0 * Math.pi * u2
                  b1 = r * Math.cos t
                  b2 = r * Math.sin t
              pure $ [ b1, b2 ]
normals :: Int -> Effect (Array Number)
normals n = do
  {-- ls <- replicateA ((n+1)/2) $ boxMuller --}
  {-- ls <- sequence (replicate n boxMuller) --}
  {-- let x = ls `debug` (show ls <> "xxxxxxxx") --}
  {-- pure $ A.take n $ A.concat ls --}
  ls0 <- boxMuller
  ls1 <- boxMuller
  ls2 <- boxMuller
  ls3 <- boxMuller
  ls4 <- boxMuller
  pure $ A.take n $ A.concat [ls0, ls1, ls2, ls3, ls4]

-- | Calculate mean and standard deviation
stats :: Array Number -> Tuple Number Number
stats xs = Tuple mean stddev where
  n = toNumber (A.length xs)
  mean = sum xs / n
  stddev = Math.sqrt $ sum (map (\v -> sqr (v-mean)) xs) / n
