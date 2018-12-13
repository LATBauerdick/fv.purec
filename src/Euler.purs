module Euler where

import Prelude

import Data.Array (filter, (..))
{-- import Data.List (List, filter, (..)) --}
import Data.Foldable (sum)

ns = 0 .. 20

multiples = filter (\n -> mod n 3 == 0 || mod n 5 == 0) ns

answer :: Int
answer = sum multiples
