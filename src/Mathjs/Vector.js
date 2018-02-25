"use strict";

// module Mathjs.Matrix

var mathjs = require("mathjs");

exports._zeros = function(x){
    return mathjs.zeros(x);
}

exports._isZero = function(x) {
    return math.isZero(x);
}

exports._ones = function(x){
    return mathjs.ones(x);
}

exports._add = function(x){
    return function (y) {
        return mathjs.add(x, y);
    }
}

exports._distance = function(x){
    return function (y) {
        return mathjs.distance(x, y);
    }
}

exports._subtract = function(x) {
    return function (y) {
        return mathjs.subtract(x, y);
    }
}

exports._dot = function(x){
    return function (y) {
        return mathjs.dot(x, y);
    }
}

exports._extract = function(e){ return e; }
