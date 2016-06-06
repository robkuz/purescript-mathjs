"use strict";

// module Mathjs.Matrix

var mathjs = require("mathjs");

exports._eye = function(x){
    return function (y) {
        return mathjs.eye(x, y);
    }
}

exports._zeros = function(x){
    return function (y) {
        return mathjs.zeros(x, y);
    }
}

exports._ones = function(x){
    return function (y) {
        return mathjs.ones(x, y);
    }
}

exports._dot = function(x){
    return function (y) {
        return mathjs.dot(x, y);
    }
}

exports._extract = function(e){ return e; }

exports._det = mathjs.det;
exports._inv = mathjs.inv;
exports._transpose = mathjs.transpose;
exports._trace = mathjs.trace;
exports._diag = mathjs.diag;
