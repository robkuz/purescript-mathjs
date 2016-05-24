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


type MatrixF = {_data :: Array Numbers, _size :: Array Int}
type Sizes = Tuple Int Int

foreign import _extract :: forall e. e -> MatrixF

foreign import _zeros :: Int -> Int -> MatrixF
foreign import _ones :: Int -> Int -> MatrixF
foreign import _eye :: Int -> Int -> MatrixF

foreign import _dot :: Numbers -> Numbers -> Number

foreign import _det :: Array Numbers -> Number
--foreign import _diag :: forall e. e -> MatrixF
--foreign import _flatten :: forall e. e -> MatrixF
--foreign import _inv :: forall e. e -> MatrixF
--foreign import _trace :: forall e. e -> MatrixF
--foreign import _transpose :: forall e. e -> MatrixF
--foreign import _cross :: forall e. e -> MatrixF


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

check (Tuple a b) = \x -> if a then Right b else Left b

dot :: Matrix -> Matrix -> Either MatrixError Number
-- uncomment the line below and the alternative dot' fn and run pulp test to see this strange error
--dot (Matrix a sa@(Tuple ax ay)) (Matrix b sb@(Tuple bx by)) = if isVector && isSameSize then Right dot' else Left failed
dot (Matrix a sa@(Tuple ax ay)) (Matrix b sb@(Tuple bx by)) = if isVector && isSameSize then Right $ dot' a b else Left $ failed
    where
        -- uncomment the line below
        -- dot' = _dot (join a) (join b)
        dot' a b = _dot (join a) (join b)
        isVector = ax == 1 && bx == 1
        isSameSize = ay == by
        failed = 
            if isVector then 
                if isSameSize then
                    UnexpectedError
                else
                    InvalidVectorSize ay by
            else
                VectorsExpected

det :: Matrix -> Either MatrixError Number
det (Matrix a (Tuple x y)) = if x == y then Right (_det a) else Left SquareMatrixExpected


