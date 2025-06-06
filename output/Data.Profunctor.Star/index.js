// Generated by purs version 0.14.7
"use strict";
var Control_Alt = require("../Control.Alt/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Plus = require("../Control.Plus/index.js");
var Data_Distributive = require("../Data.Distributive/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Functor_Invariant = require("../Data.Functor.Invariant/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Star = function (x) {
    return x;
};
var semigroupoidStar = function (dictBind) {
    return {
        compose: function (v) {
            return function (v1) {
                return function (x) {
                    return Control_Bind.bind(dictBind)(v1(x))(v);
                };
            };
        }
    };
};
var profunctorStar = function (dictFunctor) {
    return {
        dimap: function (f) {
            return function (g) {
                return function (v) {
                    var $75 = Data_Functor.map(dictFunctor)(g);
                    return function ($76) {
                        return $75(v(f($76)));
                    };
                };
            };
        }
    };
};
var strongStar = function (dictFunctor) {
    return {
        first: function (v) {
            return function (v1) {
                return Data_Functor.map(dictFunctor)(function (v2) {
                    return new Data_Tuple.Tuple(v2, v1.value1);
                })(v(v1.value0));
            };
        },
        second: function (v) {
            return function (v1) {
                return Data_Functor.map(dictFunctor)(Data_Tuple.Tuple.create(v1.value0))(v(v1.value1));
            };
        },
        Profunctor0: function () {
            return profunctorStar(dictFunctor);
        }
    };
};
var newtypeStar = {
    Coercible0: function () {
        return undefined;
    }
};
var invariantStar = function (dictInvariant) {
    return {
        imap: function (f) {
            return function (g) {
                return function (v) {
                    var $77 = Data_Functor_Invariant.imap(dictInvariant)(f)(g);
                    return function ($78) {
                        return $77(v($78));
                    };
                };
            };
        }
    };
};
var hoistStar = function (f) {
    return function (v) {
        return function ($79) {
            return f(v($79));
        };
    };
};
var functorStar = function (dictFunctor) {
    return {
        map: function (f) {
            return function (v) {
                var $80 = Data_Functor.map(dictFunctor)(f);
                return function ($81) {
                    return $80(v($81));
                };
            };
        }
    };
};
var distributiveStar = function (dictDistributive) {
    return {
        distribute: function (dictFunctor) {
            return function (f) {
                return function (a) {
                    return Data_Distributive.collect(dictDistributive)(dictFunctor)(function (v) {
                        return v(a);
                    })(f);
                };
            };
        },
        collect: function (dictFunctor) {
            return function (f) {
                var $82 = Data_Distributive.distribute(distributiveStar(dictDistributive))(dictFunctor);
                var $83 = Data_Functor.map(dictFunctor)(f);
                return function ($84) {
                    return $82($83($84));
                };
            };
        },
        Functor0: function () {
            return functorStar(dictDistributive.Functor0());
        }
    };
};
var closedStar = function (dictDistributive) {
    return {
        closed: function (v) {
            return function (g) {
                return Data_Distributive.distribute(dictDistributive)(Data_Functor.functorFn)(function ($85) {
                    return v(g($85));
                });
            };
        },
        Profunctor0: function () {
            return profunctorStar(dictDistributive.Functor0());
        }
    };
};
var choiceStar = function (dictApplicative) {
    return {
        left: function (v) {
            return Data_Either.either((function () {
                var $86 = Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Either.Left.create);
                return function ($87) {
                    return $86(v($87));
                };
            })())((function () {
                var $88 = Control_Applicative.pure(dictApplicative);
                return function ($89) {
                    return $88(Data_Either.Right.create($89));
                };
            })());
        },
        right: function (v) {
            return Data_Either.either((function () {
                var $90 = Control_Applicative.pure(dictApplicative);
                return function ($91) {
                    return $90(Data_Either.Left.create($91));
                };
            })())((function () {
                var $92 = Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Either.Right.create);
                return function ($93) {
                    return $92(v($93));
                };
            })());
        },
        Profunctor0: function () {
            return profunctorStar((dictApplicative.Apply0()).Functor0());
        }
    };
};
var categoryStar = function (dictMonad) {
    return {
        identity: Control_Applicative.pure(dictMonad.Applicative0()),
        Semigroupoid0: function () {
            return semigroupoidStar(dictMonad.Bind1());
        }
    };
};
var applyStar = function (dictApply) {
    return {
        apply: function (v) {
            return function (v1) {
                return function (a) {
                    return Control_Apply.apply(dictApply)(v(a))(v1(a));
                };
            };
        },
        Functor0: function () {
            return functorStar(dictApply.Functor0());
        }
    };
};
var bindStar = function (dictBind) {
    return {
        bind: function (v) {
            return function (f) {
                return function (x) {
                    return Control_Bind.bind(dictBind)(v(x))(function (a) {
                        var v1 = f(a);
                        return v1(x);
                    });
                };
            };
        },
        Apply0: function () {
            return applyStar(dictBind.Apply0());
        }
    };
};
var applicativeStar = function (dictApplicative) {
    return {
        pure: function (a) {
            return function (v) {
                return Control_Applicative.pure(dictApplicative)(a);
            };
        },
        Apply0: function () {
            return applyStar(dictApplicative.Apply0());
        }
    };
};
var monadStar = function (dictMonad) {
    return {
        Applicative0: function () {
            return applicativeStar(dictMonad.Applicative0());
        },
        Bind1: function () {
            return bindStar(dictMonad.Bind1());
        }
    };
};
var altStar = function (dictAlt) {
    return {
        alt: function (v) {
            return function (v1) {
                return function (a) {
                    return Control_Alt.alt(dictAlt)(v(a))(v1(a));
                };
            };
        },
        Functor0: function () {
            return functorStar(dictAlt.Functor0());
        }
    };
};
var plusStar = function (dictPlus) {
    return {
        empty: function (v) {
            return Control_Plus.empty(dictPlus);
        },
        Alt0: function () {
            return altStar(dictPlus.Alt0());
        }
    };
};
var alternativeStar = function (dictAlternative) {
    return {
        Applicative0: function () {
            return applicativeStar(dictAlternative.Applicative0());
        },
        Plus1: function () {
            return plusStar(dictAlternative.Plus1());
        }
    };
};
var monadPlusStar = function (dictMonadPlus) {
    return {
        Monad0: function () {
            return monadStar(dictMonadPlus.Monad0());
        },
        Alternative1: function () {
            return alternativeStar(dictMonadPlus.Alternative1());
        }
    };
};
var monadZeroStar = function (dictMonadZero) {
    return {
        Monad0: function () {
            return monadStar(dictMonadZero.Monad0());
        },
        Alternative1: function () {
            return alternativeStar(dictMonadZero.Alternative1());
        },
        MonadZeroIsDeprecated2: function () {
            return undefined;
        }
    };
};
module.exports = {
    Star: Star,
    hoistStar: hoistStar,
    newtypeStar: newtypeStar,
    semigroupoidStar: semigroupoidStar,
    categoryStar: categoryStar,
    functorStar: functorStar,
    invariantStar: invariantStar,
    applyStar: applyStar,
    applicativeStar: applicativeStar,
    bindStar: bindStar,
    monadStar: monadStar,
    altStar: altStar,
    plusStar: plusStar,
    alternativeStar: alternativeStar,
    monadZeroStar: monadZeroStar,
    monadPlusStar: monadPlusStar,
    distributiveStar: distributiveStar,
    profunctorStar: profunctorStar,
    strongStar: strongStar,
    choiceStar: choiceStar,
    closedStar: closedStar
};
