"use strict";

// module Mathjs.Geometry

var mathjs = require("mathjs");

exports._distance = function(p1){
    return function (p2) {
        return mathjs.distance(p1, p2);
    }
}

exports._intersect = function(a1) {
    return function(a2) {
        return function(b1) {
            return function(b2) {
                var result = mathjs.intersect(a1, a2, b1, b2);
                return  result != null ? result : [];
            }
        }
    }
}
