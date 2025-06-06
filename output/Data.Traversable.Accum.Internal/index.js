// Generated by purs version 0.14.7
"use strict";
var StateR = function (x) {
    return x;
};
var StateL = function (x) {
    return x;
};
var stateR = function (v) {
    return v;
};
var stateL = function (v) {
    return v;
};
var functorStateR = {
    map: function (f) {
        return function (k) {
            return function (s) {
                var v = stateR(k)(s);
                return {
                    accum: v.accum,
                    value: f(v.value)
                };
            };
        };
    }
};
var functorStateL = {
    map: function (f) {
        return function (k) {
            return function (s) {
                var v = stateL(k)(s);
                return {
                    accum: v.accum,
                    value: f(v.value)
                };
            };
        };
    }
};
var applyStateR = {
    apply: function (f) {
        return function (x) {
            return function (s) {
                var v = stateR(x)(s);
                var v1 = stateR(f)(v.accum);
                return {
                    accum: v1.accum,
                    value: v1.value(v.value)
                };
            };
        };
    },
    Functor0: function () {
        return functorStateR;
    }
};
var applyStateL = {
    apply: function (f) {
        return function (x) {
            return function (s) {
                var v = stateL(f)(s);
                var v1 = stateL(x)(v.accum);
                return {
                    accum: v1.accum,
                    value: v.value(v1.value)
                };
            };
        };
    },
    Functor0: function () {
        return functorStateL;
    }
};
var applicativeStateR = {
    pure: function (a) {
        return function (s) {
            return {
                accum: s,
                value: a
            };
        };
    },
    Apply0: function () {
        return applyStateR;
    }
};
var applicativeStateL = {
    pure: function (a) {
        return function (s) {
            return {
                accum: s,
                value: a
            };
        };
    },
    Apply0: function () {
        return applyStateL;
    }
};
module.exports = {
    StateL: StateL,
    stateL: stateL,
    StateR: StateR,
    stateR: stateR,
    functorStateL: functorStateL,
    applyStateL: applyStateL,
    applicativeStateL: applicativeStateL,
    functorStateR: functorStateR,
    applyStateR: applyStateR,
    applicativeStateR: applicativeStateR
};
