// Generated by purs version 0.14.7
"use strict";
var Data_Eq = require("../Data.Eq/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Ordering = require("../Data.Ordering/index.js");
var Equivalence = function (x) {
    return x;
};
var semigroupEquivalence = {
    append: function (v) {
        return function (v1) {
            return function (a) {
                return function (b) {
                    return v(a)(b) && v1(a)(b);
                };
            };
        };
    }
};
var newtypeEquivalence = {
    Coercible0: function () {
        return undefined;
    }
};
var monoidEquivalence = {
    mempty: function (v) {
        return function (v1) {
            return true;
        };
    },
    Semigroup0: function () {
        return semigroupEquivalence;
    }
};
var defaultEquivalence = function (dictEq) {
    return Data_Eq.eq(dictEq);
};
var contravariantEquivalence = {
    cmap: function (f) {
        return function (v) {
            return Data_Function.on(v)(f);
        };
    }
};
var comparisonEquivalence = function (v) {
    return function (a) {
        return function (b) {
            return Data_Eq.eq(Data_Ordering.eqOrdering)(v(a)(b))(Data_Ordering.EQ.value);
        };
    };
};
module.exports = {
    Equivalence: Equivalence,
    defaultEquivalence: defaultEquivalence,
    comparisonEquivalence: comparisonEquivalence,
    newtypeEquivalence: newtypeEquivalence,
    contravariantEquivalence: contravariantEquivalence,
    semigroupEquivalence: semigroupEquivalence,
    monoidEquivalence: monoidEquivalence
};
