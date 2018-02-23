module Mathjs.Matrix where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Either (Either(..))
import Data.Array as A
import Data.Tuple (Tuple(..))
import Data.Foldable (all)

import Mathjs.Util (Numbers)

type VectorF = {_data :: Numbers, _size :: Array Int}
type MatrixF = {_data :: Array Numbers, _size :: Array Int}
type Sizes = Tuple Int Int

foreign import _extract :: forall e. e -> MatrixF

foreign import _zeros :: Int -> Int -> MatrixF
foreign import _ones :: Int -> Int -> MatrixF
foreign import _eye :: Int -> Int -> MatrixF

foreign import _add :: Array Numbers -> Array Numbers -> Array Numbers
foreign import _subtract :: Array Numbers -> Array Numbers -> Array Numbers

foreign import _dot :: Numbers -> Numbers -> Number

foreign import _det :: Array Numbers -> Number
foreign import _inv :: Array Numbers -> Array Numbers
foreign import _trace :: Array Numbers -> Number

foreign import _transpose :: Array Numbers -> Array Numbers
foreign import _diag :: Array Numbers -> Numbers

--foreign import _cross :: forall e. e -> MatrixF

-- missing
-- multiply
-- divide

data Matrix = Matrix (Array Numbers) Sizes

data MatrixError =
        VectorsExpected
    |   InvalidVectorSize Int Int
    |   InvalidRowSize Int
    |   SquareMatrixExpected
    |   UnexpectedError

instance showMatrix :: Show Matrix where
    show (Matrix n d) = "Matrix (" <> show n <> " " <> show d <> ")"

instance eqMatrix :: Eq Matrix where
    eq (Matrix m1 s1) (Matrix m2 s2) = m1 == m2 && s1 == s2


instance eqMatrixError :: Eq MatrixError where
    eq VectorsExpected              VectorsExpected = true
    eq SquareMatrixExpected         SquareMatrixExpected = true
    eq (InvalidVectorSize ax ay)    (InvalidVectorSize bx by) = ax == bx && ay == by
    eq (InvalidRowSize x)           (InvalidRowSize y) = x == y
    eq UnexpectedError              UnexpectedError = true
    eq _ _ = false

instance showMatrixError :: Show MatrixError where
    show VectorsExpected = "(VectorsExpected)"
    show SquareMatrixExpected = "(SquareMatrixExpected)"
    show UnexpectedError = "(VectorsExpected)"
    show (InvalidVectorSize ax ay) = "(InvalidVectorSize " <> show ax <> " " <> show ay <> ")"
    show (InvalidRowSize x) = "(InvalidRowSize " <> show x <> ")"

applyOnSizes :: forall a b. ((Int -> Boolean) -> Array Int -> b) -> Array (Array a) -> b
applyOnSizes fn xs = fn (\y -> Just y == (A.head $ sizes xs)) $ sizes xs
    where
        sizes = map A.length

same :: forall a. Array (Array a) -> Boolean
same = applyOnSizes all

index :: forall a. Array (Array a) -> Array Boolean
index = applyOnSizes map

sizes :: Array Numbers -> Sizes
sizes xs = Tuple (A.length xs) (fromMaybe 0 $ A.head (map A.length xs))

make :: Array Numbers -> Either MatrixError Matrix
make a = if same a then Right $ Matrix a $ sizes a else fails a
    where
        indexOfErr = A.findIndex ((==) false) <<< index
        fails xs = Left $ maybe UnexpectedError InvalidRowSize $ indexOfErr xs


getData :: Matrix -> Array Numbers
getData (Matrix m _) = m

getSizes :: Matrix -> Sizes
getSizes (Matrix _ s) = s

fromMatrixF :: MatrixF -> Matrix
fromMatrixF m = Matrix m._data $ sizes m._data

fromArray :: Array Numbers -> Matrix
fromArray a = Matrix a $ sizes a

eye :: Int -> Int -> Matrix
eye x y = fromMatrixF $ _eye x y

eye' :: Int -> Matrix
eye' x = eye x x

zeros :: Int -> Int -> Matrix
zeros x y = fromMatrixF $ _zeros x y

zeros' :: Int -> Matrix
zeros' x = zeros x x

ones :: Int -> Int -> Matrix
ones x y = fromMatrixF $ _ones x y

ones' :: Int -> Matrix
ones' x = ones x x

add :: Matrix -> Matrix -> Either MatrixError Matrix
add (Matrix a (Tuple ax ay)) (Matrix b (Tuple bx by))
    | ay /= by              = Left  $ InvalidVectorSize ay by
    | otherwise             = Right $ fromArray $ _add a b

subtract :: Matrix -> Matrix -> Either MatrixError Matrix
subtract (Matrix a (Tuple ax ay)) (Matrix b (Tuple bx by))
    | ay /= by              = Left  $ InvalidVectorSize ay by
    | otherwise             = Right $ fromArray $ _subtract a b

dot :: Matrix -> Matrix -> Either MatrixError Number
dot (Matrix a (Tuple ax ay)) (Matrix b (Tuple bx by))
    | ax /= 1 && bx /= 1    = Left  $ VectorsExpected
    | ay /= by              = Left  $ InvalidVectorSize ay by
    | otherwise             = Right $ _dot (join a) (join b)


squareMatrixFnStub :: forall a. (Array (Array Number) -> a) -> Matrix -> Either MatrixError a
squareMatrixFnStub f (Matrix a (Tuple x y)) = if x == y then Right (f a) else Left SquareMatrixExpected

det :: Matrix -> Either MatrixError Number
det = squareMatrixFnStub _det

inv :: Matrix -> Either MatrixError Matrix
inv = squareMatrixFnStub (fromArray <<< _inv)

trace :: Matrix -> Either MatrixError Number
trace = squareMatrixFnStub _trace

transpose :: Matrix -> Matrix
transpose (Matrix a _) = fromArray $ _transpose a

flatten :: Matrix -> Matrix
flatten (Matrix a _) = fromArray $ [A.concat a]

diag :: Matrix -> Either MatrixError Matrix
diag = squareMatrixFnStub (fromArray <<< A.singleton <<< _diag)
