// Generated by purs version 0.14.7
"use strict";
var Data_Functor_Product = require("../Data.Functor.Product/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var product9 = function (a) {
    return function (b) {
        return function (c) {
            return function (d) {
                return function (e) {
                    return function (f) {
                        return function (g) {
                            return function (h) {
                                return function (i) {
                                    return Data_Functor_Product.product(a)(Data_Functor_Product.product(b)(Data_Functor_Product.product(c)(Data_Functor_Product.product(d)(Data_Functor_Product.product(e)(Data_Functor_Product.product(f)(Data_Functor_Product.product(g)(Data_Functor_Product.product(h)(Data_Functor_Product.product(i)(Data_Unit.unit)))))))));
                                };
                            };
                        };
                    };
                };
            };
        };
    };
};
var product8 = function (a) {
    return function (b) {
        return function (c) {
            return function (d) {
                return function (e) {
                    return function (f) {
                        return function (g) {
                            return function (h) {
                                return Data_Functor_Product.product(a)(Data_Functor_Product.product(b)(Data_Functor_Product.product(c)(Data_Functor_Product.product(d)(Data_Functor_Product.product(e)(Data_Functor_Product.product(f)(Data_Functor_Product.product(g)(Data_Functor_Product.product(h)(Data_Unit.unit))))))));
                            };
                        };
                    };
                };
            };
        };
    };
};
var product7 = function (a) {
    return function (b) {
        return function (c) {
            return function (d) {
                return function (e) {
                    return function (f) {
                        return function (g) {
                            return Data_Functor_Product.product(a)(Data_Functor_Product.product(b)(Data_Functor_Product.product(c)(Data_Functor_Product.product(d)(Data_Functor_Product.product(e)(Data_Functor_Product.product(f)(Data_Functor_Product.product(g)(Data_Unit.unit)))))));
                        };
                    };
                };
            };
        };
    };
};
var product6 = function (a) {
    return function (b) {
        return function (c) {
            return function (d) {
                return function (e) {
                    return function (f) {
                        return Data_Functor_Product.product(a)(Data_Functor_Product.product(b)(Data_Functor_Product.product(c)(Data_Functor_Product.product(d)(Data_Functor_Product.product(e)(Data_Functor_Product.product(f)(Data_Unit.unit))))));
                    };
                };
            };
        };
    };
};
var product5 = function (a) {
    return function (b) {
        return function (c) {
            return function (d) {
                return function (e) {
                    return Data_Functor_Product.product(a)(Data_Functor_Product.product(b)(Data_Functor_Product.product(c)(Data_Functor_Product.product(d)(Data_Functor_Product.product(e)(Data_Unit.unit)))));
                };
            };
        };
    };
};
var product4 = function (a) {
    return function (b) {
        return function (c) {
            return function (d) {
                return Data_Functor_Product.product(a)(Data_Functor_Product.product(b)(Data_Functor_Product.product(c)(Data_Functor_Product.product(d)(Data_Unit.unit))));
            };
        };
    };
};
var product3 = function (a) {
    return function (b) {
        return function (c) {
            return Data_Functor_Product.product(a)(Data_Functor_Product.product(b)(Data_Functor_Product.product(c)(Data_Unit.unit)));
        };
    };
};
var product2 = function (a) {
    return function (b) {
        return Data_Functor_Product.product(a)(Data_Functor_Product.product(b)(Data_Unit.unit));
    };
};
var product10 = function (a) {
    return function (b) {
        return function (c) {
            return function (d) {
                return function (e) {
                    return function (f) {
                        return function (g) {
                            return function (h) {
                                return function (i) {
                                    return function (j) {
                                        return Data_Functor_Product.product(a)(Data_Functor_Product.product(b)(Data_Functor_Product.product(c)(Data_Functor_Product.product(d)(Data_Functor_Product.product(e)(Data_Functor_Product.product(f)(Data_Functor_Product.product(g)(Data_Functor_Product.product(h)(Data_Functor_Product.product(i)(Data_Functor_Product.product(j)(Data_Unit.unit))))))))));
                                    };
                                };
                            };
                        };
                    };
                };
            };
        };
    };
};
var product1 = function (a) {
    return Data_Functor_Product.product(a)(Data_Unit.unit);
};
var get9 = function (v) {
    return v.value1.value1.value1.value1.value1.value1.value1.value1.value0;
};
var get8 = function (v) {
    return v.value1.value1.value1.value1.value1.value1.value1.value0;
};
var get7 = function (v) {
    return v.value1.value1.value1.value1.value1.value1.value0;
};
var get6 = function (v) {
    return v.value1.value1.value1.value1.value1.value0;
};
var get5 = function (v) {
    return v.value1.value1.value1.value1.value0;
};
var get4 = function (v) {
    return v.value1.value1.value1.value0;
};
var get3 = function (v) {
    return v.value1.value1.value0;
};
var get2 = function (v) {
    return v.value1.value0;
};
var get10 = function (v) {
    return v.value1.value1.value1.value1.value1.value1.value1.value1.value1.value0;
};
var get1 = function (v) {
    return v.value0;
};
module.exports = {
    product1: product1,
    product2: product2,
    product3: product3,
    product4: product4,
    product5: product5,
    product6: product6,
    product7: product7,
    product8: product8,
    product9: product9,
    product10: product10,
    get1: get1,
    get2: get2,
    get3: get3,
    get4: get4,
    get5: get5,
    get6: get6,
    get7: get7,
    get8: get8,
    get9: get9,
    get10: get10
};
