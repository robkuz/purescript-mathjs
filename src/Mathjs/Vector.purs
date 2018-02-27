module Mathjs.Vector where

import Prelude

import Data.Either (Either(..))
import Data.Array as A
import Data.Tuple (Tuple(..))

type VectorF = {_data :: Array Number, _size :: Array Int}
type Sizes = Tuple Int Int

foreign import _extract :: forall e. e -> VectorF

foreign import _zeros :: Int -> VectorF
foreign import _isZero :: Array Number -> Boolean
foreign import _ones :: Int -> VectorF

foreign import _add :: Array Number -> Array Number -> Array Number
foreign import _subtract :: Array Number -> Array Number -> Array Number
foreign import _distance :: Array Number -> Array Number -> Number

foreign import _dot :: Array Number -> Array Number -> Number

--foreign import _cross :: forall e. e -> VectorF

-- missing
-- multiply
-- divide

data Vector = Vector (Array Number) Sizes

data VectorError =
    InvalidVectorSize Int Int
    |   InvalidRowSize Int
    |   UnexpectedError

instance showVector :: Show Vector where
    show (Vector n d) = "Vector (" <> show n <> " " <> show d <> ")"

instance eqVector :: Eq Vector where
    eq (Vector m1 s1) (Vector m2 s2) = m1 == m2 && s1 == s2

instance eqVectorError :: Eq VectorError where
    eq (InvalidVectorSize ax ay) (InvalidVectorSize bx by) = ax == bx && ay == by
    eq (InvalidRowSize x) (InvalidRowSize y) = x == y
    eq UnexpectedError UnexpectedError = true
    eq _ _ = false

-- applyOnSizes :: forall a b. ((Int -> Boolean) -> Array Int -> b) -> Array (Array a) -> b
-- applyOnSizes fn xs = fn (\y -> Just y == (A.head $ sizes xs)) $ sizes xs
--     where
--         sizes = map A.length
--
-- index :: forall a. Array (Array a) -> Array Boolean
-- index = applyOnSizes map

sizes :: Array Number -> Sizes
sizes xs = Tuple 1 (A.length xs)

make :: Array Number -> Vector
make a = Vector a $ sizes a

getData :: Vector -> Array Number
getData (Vector m _) = m

getSizes :: Vector -> Sizes
getSizes (Vector _ s) = s

fromVectorF :: VectorF -> Vector
fromVectorF m = Vector m._data $ sizes m._data

fromArray :: Array Number -> Vector
fromArray a = Vector a $ sizes a

zeros :: Int -> Vector
zeros = fromVectorF <<< _zeros

isZero :: Vector -> Boolean
isZero (Vector a (Tuple _ y)) = _isZero a

ones :: Int -> Vector
ones = fromVectorF <<< _ones

dim :: Vector -> Int
dim (Vector _ (Tuple _ y)) = y

magnitude :: Vector -> Either VectorError Number
magnitude v = distance (zeros $ dim v) v

normalize :: Vector -> Either VectorError Vector
normalize v = case isZero v of
    true -> pure $ zeros $ dim v
    false -> do
      m <- magnitude v
      divide v m


divide :: Vector -> Number -> Either VectorError Vector
divide (Vector a b) n
  | n == 0.0 = Left UnexpectedError
  | otherwise = Right $ Vector ((\a -> a / n) <$> a) b

vvv :: (Array Number -> Array Number -> Array Number) -> Vector -> Vector -> Either VectorError Vector
vvv op (Vector a (Tuple _ ay)) (Vector b (Tuple _ by))
  | ay /= by = Left $ InvalidVectorSize ay by
  | otherwise = Right $ fromArray $ op a b

add :: Vector -> Vector -> Either VectorError Vector
add = vvv _add

subtract :: Vector -> Vector -> Either VectorError Vector
subtract = vvv _subtract

distance :: Vector -> Vector -> Either VectorError Number
distance (Vector a (Tuple _ ay)) (Vector b (Tuple _ by))
    | ay /= by = Left  $ InvalidVectorSize ay by
    | otherwise = Right $ _distance a b

dot :: Vector -> Vector -> Either VectorError Number
dot (Vector a (Tuple ax ay)) (Vector b (Tuple bx by))
    | ay /= by              = Left  $ InvalidVectorSize ay by
    | otherwise             = Right $ _dot a b
