// Generated by purs version 0.14.7
"use strict";
var Safe_Coerce = require("../Safe.Coerce/index.js");
var wrap = Safe_Coerce.coerce;
var unwrap = Safe_Coerce.coerce;
var underF2 = function () {
    return function () {
        return function () {
            return function () {
                return function (v) {
                    return Safe_Coerce.coerce();
                };
            };
        };
    };
};
var underF = function () {
    return function () {
        return function () {
            return function () {
                return function (v) {
                    return Safe_Coerce.coerce();
                };
            };
        };
    };
};
var under2 = function () {
    return function () {
        return function (v) {
            return Safe_Coerce.coerce();
        };
    };
};
var under = function () {
    return function () {
        return function (v) {
            return Safe_Coerce.coerce();
        };
    };
};
var un = function () {
    return function (v) {
        return unwrap();
    };
};
var traverse = function () {
    return function () {
        return function (v) {
            return Safe_Coerce.coerce();
        };
    };
};
var overF2 = function () {
    return function () {
        return function () {
            return function () {
                return function (v) {
                    return Safe_Coerce.coerce();
                };
            };
        };
    };
};
var overF = function () {
    return function () {
        return function () {
            return function () {
                return function (v) {
                    return Safe_Coerce.coerce();
                };
            };
        };
    };
};
var over2 = function () {
    return function () {
        return function (v) {
            return Safe_Coerce.coerce();
        };
    };
};
var over = function () {
    return function () {
        return function (v) {
            return Safe_Coerce.coerce();
        };
    };
};
var newtypeMultiplicative = {
    Coercible0: function () {
        return undefined;
    }
};
var newtypeLast = {
    Coercible0: function () {
        return undefined;
    }
};
var newtypeFirst = {
    Coercible0: function () {
        return undefined;
    }
};
var newtypeEndo = {
    Coercible0: function () {
        return undefined;
    }
};
var newtypeDual = {
    Coercible0: function () {
        return undefined;
    }
};
var newtypeDisj = {
    Coercible0: function () {
        return undefined;
    }
};
var newtypeConj = {
    Coercible0: function () {
        return undefined;
    }
};
var newtypeAdditive = {
    Coercible0: function () {
        return undefined;
    }
};
var collect = function () {
    return function () {
        return function (v) {
            return Safe_Coerce.coerce();
        };
    };
};
var alaF = function () {
    return function () {
        return function () {
            return function () {
                return function (v) {
                    return Safe_Coerce.coerce();
                };
            };
        };
    };
};
var ala = function () {
    return function () {
        return function () {
            return function (v) {
                return function (f) {
                    return Safe_Coerce.coerce()(f(wrap()));
                };
            };
        };
    };
};
module.exports = {
    wrap: wrap,
    unwrap: unwrap,
    un: un,
    ala: ala,
    alaF: alaF,
    over: over,
    overF: overF,
    under: under,
    underF: underF,
    over2: over2,
    overF2: overF2,
    under2: under2,
    underF2: underF2,
    traverse: traverse,
    collect: collect,
    newtypeAdditive: newtypeAdditive,
    newtypeMultiplicative: newtypeMultiplicative,
    newtypeConj: newtypeConj,
    newtypeDisj: newtypeDisj,
    newtypeDual: newtypeDual,
    newtypeEndo: newtypeEndo,
    newtypeFirst: newtypeFirst,
    newtypeLast: newtypeLast
};
