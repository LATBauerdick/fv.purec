module Prelude.Extended (
    module Prelude
  , List (..), range, fromList
  , fromIntegral
  {-- , words, unwords, unlines --}
  , sqr, mod'
  , irem, iflt
  , prettyMatrix
  {-- , normals, stats --}
  , debug, trace
  , uidx, uJust
  , to1fix, to2fix, to3fix, to5fix
  , error
  , undefined
  ) where

import Prelude
import Effect ( Effect )
import Effect.Console ( log )
import Data.Int ( round, toNumber, floor )
import Partial.Unsafe (unsafePartial, unsafeCrashWith)
import Unsafe.Coerce ( unsafeCoerce ) as Unsafe.Coerce
import Data.Foldable ( class Foldable, foldr, sum, maximum )
import Data.List ( List(..), (:), range, fromFoldable ) as L
import Data.Array ( unsafeIndex, range, length, fromFoldable, replicate
  ) as A
import Data.Tuple ( Tuple(..), fst, snd )
import Data.Maybe ( Maybe(..), fromMaybe', fromMaybe, fromJust )
{-- import Data.String.CodeUnits ( fromCharArray ) --}
import Data.String ( length, fromCharArray ) as S
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

error :: forall a. String -> a
error = unsafeCrashWith
undefined :: forall a. a
undefined = Unsafe.Coerce.unsafeCoerce unit

trace :: forall a. String -> a -> a
trace s a = const a (unsafePerformEffect (log s))

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
--  ls = L.Cons "neuneuneu" L.Nil
  ls = do
    i <- range 1 r
    let ws :: List String
        ws = map (\j -> fillBlanks mx (to3fix $ unsafeGet i j v)) (range 1 c)
    pure $ "( " <> unwords ws <> " )"
  mx = fromMaybe 0 (maximum $ map (S.length <<< to3fix) v)
  fillBlanks k str =
    (S.fromCharArray $ A.replicate (k - S.length str) ' ') <> str

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
to0fix = show --format (width 4 <> precision 0)
to1fix :: Number -> String
to1fix = show --format (width 6 <> precision 1)
to2fix :: Number -> String
to2fix = show --format (width 7 <> precision 2)
to3fix :: Number -> String
to3fix = show --format (width 8 <> precision 3)
to5fix :: Number -> String
to5fix = show --format (width 10 <> precision 5)

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

