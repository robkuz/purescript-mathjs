module Mathjs.Matrix where

import Prelude

import Data.Maybe
import Data.Either
import Data.Array
import Data.Tuple
import Data.Function
import Data.Foldable

import Control.Bind
import Control.Monad.Eff

import Mathjs.Util

import Control.Monad.Eff.Random


type MatrixF = {_data :: Array Numbers, _size :: Array Int}
type Sizes = Tuple Int Int

foreign import _extract :: forall e. e -> MatrixF

foreign import _zeros :: Int -> Int -> MatrixF
foreign import _ones :: Int -> Int -> MatrixF
foreign import _eye :: Int -> Int -> MatrixF

foreign import _dot :: Numbers -> Numbers -> Number

foreign import _det :: Array Numbers -> Number
foreign import _inv :: Array Numbers -> Array Numbers
foreign import _trace :: Array Numbers -> Number

foreign import _transpose :: Array Numbers -> Array Numbers
foreign import _diag :: Array Numbers -> Numbers

--foreign import _cross :: forall e. e -> MatrixF

-- missing
-- add
-- subtract
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
    show (InvalidVectorSize ax ay) = "(InvalidVectorSize " ++ show ax ++ " " ++ show ay ++ ")"
    show (InvalidRowSize x) = "(InvalidRowSize " ++ show x ++ ")"

applyOnSizes fn xs = fn (\y -> Just y == (head $ sizes xs)) $ sizes xs
    where
        sizes = map length

same = applyOnSizes all
index = applyOnSizes map        

sizes :: Array Numbers -> Sizes
sizes xs = Tuple (length xs) (fromMaybe 0 $ head (map length xs))        

make :: Array Numbers -> Either MatrixError Matrix
make a = if same a then Right $ Matrix a $ sizes a else fails a
    where 
        indexOfErr = findIndex (false ==) <<< index
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

dot :: Matrix -> Matrix -> Either MatrixError Number
dot (Matrix a (Tuple ax ay)) (Matrix b (Tuple bx by)) 
    | ax /= 1 && bx /= 1    = Left  $ VectorsExpected
    | ay /= by              = Left  $ InvalidVectorSize ay by
    | otherwise             = Right $ _dot (join a) (join b)
    

squareMatrixFnStub f = \(Matrix a (Tuple x y)) -> if x == y then Right (f a) else Left SquareMatrixExpected

det :: Matrix -> Either MatrixError Number
det = squareMatrixFnStub _det

inv :: Matrix -> Either MatrixError Matrix
inv = squareMatrixFnStub (fromArray <<< _inv)

trace :: Matrix -> Either MatrixError Number
trace = squareMatrixFnStub _trace

transpose :: Matrix -> Matrix
transpose (Matrix a _) = fromArray $ _transpose a

flatten :: Matrix -> Matrix
flatten (Matrix a _) = fromArray $ [concat a]

diag :: Matrix -> Either MatrixError Matrix
diag = squareMatrixFnStub (fromArray <<< singleton <<< _diag)

