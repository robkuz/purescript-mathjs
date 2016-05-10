"use strict";

// module Mathjs.Geometry

var mathjs = require("../../bower_components/mathjs/dist/math.min.js");

exports.range = function(from){
    return function (to) {
        return mathjs.range(from, to)._data;
    }
}

exports.zeros = function(count){ return mathjs.zeros(count)._data }

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
