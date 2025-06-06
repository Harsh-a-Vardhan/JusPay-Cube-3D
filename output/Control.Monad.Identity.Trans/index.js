// Generated by purs version 0.14.7
"use strict";
var Data_Eq = require("../Data.Eq/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var IdentityT = function (x) {
    return x;
};
var traversableIdentityT = function (dictTraversable) {
    return dictTraversable;
};
var runIdentityT = function (v) {
    return v;
};
var plusIdentityT = function (dictPlus) {
    return dictPlus;
};
var newtypeIdentityT = {
    Coercible0: function () {
        return undefined;
    }
};
var monadZeroIdentityT = function (dictMonadZero) {
    return dictMonadZero;
};
var monadWriterIdentityT = function (dictMonadWriter) {
    return dictMonadWriter;
};
var monadTransIdentityT = {
    lift: function (dictMonad) {
        return IdentityT;
    }
};
var monadThrowIdentityT = function (dictMonadThrow) {
    return dictMonadThrow;
};
var monadTellIdentityT = function (dictMonadTell) {
    return dictMonadTell;
};
var monadStateIdentityT = function (dictMonadState) {
    return dictMonadState;
};
var monadRecIdentityT = function (dictMonadRec) {
    return dictMonadRec;
};
var monadReaderIdentityT = function (dictMonadReader) {
    return dictMonadReader;
};
var monadPlusIdentityT = function (dictMonadPlus) {
    return dictMonadPlus;
};
var monadIdentityT = function (dictMonad) {
    return dictMonad;
};
var monadErrorIdentityT = function (dictMonadError) {
    return dictMonadError;
};
var monadEffectIdentityT = function (dictMonadEffect) {
    return dictMonadEffect;
};
var monadContIdentityT = function (dictMonadCont) {
    return dictMonadCont;
};
var monadAskIdentityT = function (dictMonadAsk) {
    return dictMonadAsk;
};
var mapIdentityT = function (f) {
    return function (v) {
        return f(v);
    };
};
var functorIdentityT = function (dictFunctor) {
    return dictFunctor;
};
var foldableIdentityT = function (dictFoldable) {
    return dictFoldable;
};
var eqIdentityT = function (dictEq1) {
    return function (dictEq) {
        return {
            eq: function (x) {
                return function (y) {
                    return Data_Eq.eq1(dictEq1)(dictEq)(x)(y);
                };
            }
        };
    };
};
var ordIdentityT = function (dictOrd1) {
    return function (dictOrd) {
        return {
            compare: function (x) {
                return function (y) {
                    return Data_Ord.compare1(dictOrd1)(dictOrd)(x)(y);
                };
            },
            Eq0: function () {
                return eqIdentityT(dictOrd1.Eq10())(dictOrd.Eq0());
            }
        };
    };
};
var eq1IdentityT = function (dictEq1) {
    return {
        eq1: function (dictEq) {
            return Data_Eq.eq(eqIdentityT(dictEq1)(dictEq));
        }
    };
};
var ord1IdentityT = function (dictOrd1) {
    return {
        compare1: function (dictOrd) {
            return Data_Ord.compare(ordIdentityT(dictOrd1)(dictOrd));
        },
        Eq10: function () {
            return eq1IdentityT(dictOrd1.Eq10());
        }
    };
};
var bindIdentityT = function (dictBind) {
    return dictBind;
};
var applyIdentityT = function (dictApply) {
    return dictApply;
};
var applicativeIdentityT = function (dictApplicative) {
    return dictApplicative;
};
var alternativeIdentityT = function (dictAlternative) {
    return dictAlternative;
};
var altIdentityT = function (dictAlt) {
    return dictAlt;
};
module.exports = {
    IdentityT: IdentityT,
    runIdentityT: runIdentityT,
    mapIdentityT: mapIdentityT,
    eqIdentityT: eqIdentityT,
    ordIdentityT: ordIdentityT,
    eq1IdentityT: eq1IdentityT,
    ord1IdentityT: ord1IdentityT,
    newtypeIdentityT: newtypeIdentityT,
    functorIdentityT: functorIdentityT,
    applyIdentityT: applyIdentityT,
    applicativeIdentityT: applicativeIdentityT,
    altIdentityT: altIdentityT,
    plusIdentityT: plusIdentityT,
    alternativeIdentityT: alternativeIdentityT,
    bindIdentityT: bindIdentityT,
    monadIdentityT: monadIdentityT,
    monadRecIdentityT: monadRecIdentityT,
    monadZeroIdentityT: monadZeroIdentityT,
    monadPlusIdentityT: monadPlusIdentityT,
    monadTransIdentityT: monadTransIdentityT,
    monadEffectIdentityT: monadEffectIdentityT,
    monadContIdentityT: monadContIdentityT,
    monadThrowIdentityT: monadThrowIdentityT,
    monadErrorIdentityT: monadErrorIdentityT,
    monadAskIdentityT: monadAskIdentityT,
    monadReaderIdentityT: monadReaderIdentityT,
    monadStateIdentityT: monadStateIdentityT,
    monadTellIdentityT: monadTellIdentityT,
    monadWriterIdentityT: monadWriterIdentityT,
    foldableIdentityT: foldableIdentityT,
    traversableIdentityT: traversableIdentityT
};
