// Generated by purs version 0.14.7
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Show = require("../Data.Show/index.js");
var Joker = function (x) {
    return x;
};
var showJoker = function (dictShow) {
    return {
        show: function (v) {
            return "(Joker " + (Data_Show.show(dictShow)(v) + ")");
        }
    };
};
var profunctorJoker = function (dictFunctor) {
    return {
        dimap: function (f) {
            return function (g) {
                return function (v) {
                    return Data_Functor.map(dictFunctor)(g)(v);
                };
            };
        }
    };
};
var ordJoker = function (dictOrd) {
    return dictOrd;
};
var newtypeJoker = {
    Coercible0: function () {
        return undefined;
    }
};
var hoistJoker = function (f) {
    return function (v) {
        return f(v);
    };
};
var functorJoker = function (dictFunctor) {
    return {
        map: function (f) {
            return function (v) {
                return Data_Functor.map(dictFunctor)(f)(v);
            };
        }
    };
};
var eqJoker = function (dictEq) {
    return dictEq;
};
var choiceJoker = function (dictFunctor) {
    return {
        left: function (v) {
            return Data_Functor.map(dictFunctor)(Data_Either.Left.create)(v);
        },
        right: function (v) {
            return Data_Functor.map(dictFunctor)(Data_Either.Right.create)(v);
        },
        Profunctor0: function () {
            return profunctorJoker(dictFunctor);
        }
    };
};
var bifunctorJoker = function (dictFunctor) {
    return {
        bimap: function (v) {
            return function (g) {
                return function (v1) {
                    return Data_Functor.map(dictFunctor)(g)(v1);
                };
            };
        }
    };
};
var biapplyJoker = function (dictApply) {
    return {
        biapply: function (v) {
            return function (v1) {
                return Control_Apply.apply(dictApply)(v)(v1);
            };
        },
        Bifunctor0: function () {
            return bifunctorJoker(dictApply.Functor0());
        }
    };
};
var biapplicativeJoker = function (dictApplicative) {
    return {
        bipure: function (v) {
            return function (b) {
                return Control_Applicative.pure(dictApplicative)(b);
            };
        },
        Biapply0: function () {
            return biapplyJoker(dictApplicative.Apply0());
        }
    };
};
var applyJoker = function (dictApply) {
    return {
        apply: function (v) {
            return function (v1) {
                return Control_Apply.apply(dictApply)(v)(v1);
            };
        },
        Functor0: function () {
            return functorJoker(dictApply.Functor0());
        }
    };
};
var bindJoker = function (dictBind) {
    return {
        bind: function (v) {
            return function (amb) {
                return Control_Bind.bind(dictBind)(v)((function () {
                    var $46 = Data_Newtype.un()(Joker);
                    return function ($47) {
                        return $46(amb($47));
                    };
                })());
            };
        },
        Apply0: function () {
            return applyJoker(dictBind.Apply0());
        }
    };
};
var applicativeJoker = function (dictApplicative) {
    return {
        pure: (function () {
            var $48 = Control_Applicative.pure(dictApplicative);
            return function ($49) {
                return Joker($48($49));
            };
        })(),
        Apply0: function () {
            return applyJoker(dictApplicative.Apply0());
        }
    };
};
var monadJoker = function (dictMonad) {
    return {
        Applicative0: function () {
            return applicativeJoker(dictMonad.Applicative0());
        },
        Bind1: function () {
            return bindJoker(dictMonad.Bind1());
        }
    };
};
module.exports = {
    Joker: Joker,
    hoistJoker: hoistJoker,
    newtypeJoker: newtypeJoker,
    eqJoker: eqJoker,
    ordJoker: ordJoker,
    showJoker: showJoker,
    functorJoker: functorJoker,
    applyJoker: applyJoker,
    applicativeJoker: applicativeJoker,
    bindJoker: bindJoker,
    monadJoker: monadJoker,
    bifunctorJoker: bifunctorJoker,
    biapplyJoker: biapplyJoker,
    biapplicativeJoker: biapplicativeJoker,
    profunctorJoker: profunctorJoker,
    choiceJoker: choiceJoker
};
