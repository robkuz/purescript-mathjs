module Mathjs.Geometry where

import Prelude (class Eq, class Show, ($), const, (<*>), (<$>), (/=), (==), (&&), show, (<>))

import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Either (Either(Right, Left), either)
import Data.Array (drop, take, head, length)

import Control.Bind (join)

import Mathjs.Util

foreign import _distance :: Numbers -> Numbers -> Number
foreign import _intersect :: Numbers -> Numbers -> Numbers -> Numbers -> Numbers

data Point2D = Point2D Number Number
data Line = Line Point2D Point2D

class ArrayRepresent a b where
    toArray :: a -> Array b

instance showPoint2D :: Show Point2D where
    show (Point2D x y) = "Point2D (" <> show x <> " " <> show y <> ")"

instance showLine :: Show Line where
    show (Line p1 p2) = "Line (" <> show p1 <> " " <> show p2 <> ")"

instance arrayRepresentPoint2D :: ArrayRepresent Point2D Number where
    toArray (Point2D x y) = [x, y]

instance arrayRepresentLine :: ArrayRepresent Line (Array Number) where
    toArray (Line p1 p2) = [toArray p1, toArray p2]

instance eqPoint2D :: Eq Point2D where
    eq (Point2D x1 y1) (Point2D x2 y2) = x1 == x2 && y1 == y2

defaultPoint :: Point2D
defaultPoint = Point2D 0.0 0.0
defaultLine :: Line
defaultLine = Line defaultPoint defaultPoint


checkSizeAndTransform :: forall a b. Int -> Array a -> (Array a -> b) -> Either InvalidSize b
checkSizeAndTransform size xs t = if length xs /= size then Left $ InvalidSize $ length xs else Right $ t xs

toPoint :: Array Number -> Either InvalidSize Point2D
toPoint xs = checkSizeAndTransform 2 xs toPoint'
    where
        toPoint' xs = do
            let x = head xs
            let y = head $ drop 1 xs
            let p = Point2D <$> x <*> y
            fromMaybe defaultPoint p

toLine :: Array Number -> Either InvalidSize Line
toLine xs = join $ checkSizeAndTransform 4 xs toLine'
    where 
        toLine' xs = do
            let a = take 2 xs
            let b = take 2 $ drop 2 xs
            Line <$> toPoint a <*> toPoint b


distance :: Point2D -> Point2D -> Number
distance p1 p2 = _distance (toArray p1) (toArray p2)

intersect :: Line -> Line -> Maybe Point2D
intersect (Line a1 a2) (Line b1 b2) = either (const Nothing) Just $ (toPoint $ _intersect (toArray a1) (toArray a2) (toArray b1) (toArray b2))
