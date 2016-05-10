module Test.Main where

import Prelude

import Data.Maybe 

import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.Class

import Test.Unit (test, suite)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert

import Mathjs.Geometry

main = runTest do
    suite "Geometry" do
        test "distance" do
            let p1 = Point2D 0.0 0.0
            let p2 = Point2D 4.0 3.0
            let p3 = Point2D 0.0 0.0
            Assert.assert "should be 5.0" $ distance p1 p2 == 5.0
            Assert.assert "should be 0.0" $ distance p3 p1 == 0.0
        test "intersection" do
            let l1 = Line (Point2D 3.0 6.0) (Point2D 4.0 7.0)
            let l2 = Line (Point2D 2.0 8.0) (Point2D 10.0 8.0)
            let l3 = Line (Point2D 2.0 8.0) (Point2D 10.0 8.0) -- same as above
            Assert.assert "should be intersect at 5.0 8.0" $ (intersect l1 l2) == Just (Point2D 5.0 8.0)
            Assert.assert "should not intersect" $ (intersect l2 l2) == Nothing

