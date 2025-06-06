// Generated by purs version 0.14.7
"use strict";
var $foreign = require("./foreign.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Monad_Error_Class = require("../Control.Monad.Error.Class/index.js");
var Control_Monad_Except_Trans = require("../Control.Monad.Except.Trans/index.js");
var Data_Boolean = require("../Data.Boolean/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Int = require("../Data.Int/index.js");
var Data_List_NonEmpty = require("../Data.List.NonEmpty/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ordering = require("../Data.Ordering/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_String_CodeUnits = require("../Data.String.CodeUnits/index.js");
var Unsafe_Coerce = require("../Unsafe.Coerce/index.js");
var ForeignError = (function () {
    function ForeignError(value0) {
        this.value0 = value0;
    };
    ForeignError.create = function (value0) {
        return new ForeignError(value0);
    };
    return ForeignError;
})();
var TypeMismatch = (function () {
    function TypeMismatch(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    TypeMismatch.create = function (value0) {
        return function (value1) {
            return new TypeMismatch(value0, value1);
        };
    };
    return TypeMismatch;
})();
var ErrorAtIndex = (function () {
    function ErrorAtIndex(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    ErrorAtIndex.create = function (value0) {
        return function (value1) {
            return new ErrorAtIndex(value0, value1);
        };
    };
    return ErrorAtIndex;
})();
var ErrorAtProperty = (function () {
    function ErrorAtProperty(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    ErrorAtProperty.create = function (value0) {
        return function (value1) {
            return new ErrorAtProperty(value0, value1);
        };
    };
    return ErrorAtProperty;
})();
var unsafeToForeign = Unsafe_Coerce.unsafeCoerce;
var unsafeFromForeign = Unsafe_Coerce.unsafeCoerce;
var showForeignError = {
    show: function (v) {
        if (v instanceof ForeignError) {
            return "(ForeignError " + (Data_Show.show(Data_Show.showString)(v.value0) + ")");
        };
        if (v instanceof ErrorAtIndex) {
            return "(ErrorAtIndex " + (Data_Show.show(Data_Show.showInt)(v.value0) + (" " + (Data_Show.show(showForeignError)(v.value1) + ")")));
        };
        if (v instanceof ErrorAtProperty) {
            return "(ErrorAtProperty " + (Data_Show.show(Data_Show.showString)(v.value0) + (" " + (Data_Show.show(showForeignError)(v.value1) + ")")));
        };
        if (v instanceof TypeMismatch) {
            return "(TypeMismatch " + (Data_Show.show(Data_Show.showString)(v.value0) + (" " + (Data_Show.show(Data_Show.showString)(v.value1) + ")")));
        };
        throw new Error("Failed pattern match at Foreign (line 64, column 1 - line 68, column 89): " + [ v.constructor.name ]);
    }
};
var renderForeignError = function (v) {
    if (v instanceof ForeignError) {
        return v.value0;
    };
    if (v instanceof ErrorAtIndex) {
        return "Error at array index " + (Data_Show.show(Data_Show.showInt)(v.value0) + (": " + renderForeignError(v.value1)));
    };
    if (v instanceof ErrorAtProperty) {
        return "Error at property " + (Data_Show.show(Data_Show.showString)(v.value0) + (": " + renderForeignError(v.value1)));
    };
    if (v instanceof TypeMismatch) {
        return "Type mismatch: expected " + (v.value0 + (", found " + v.value1));
    };
    throw new Error("Failed pattern match at Foreign (line 73, column 1 - line 73, column 45): " + [ v.constructor.name ]);
};
var readUndefined = function (dictMonad) {
    return function (value) {
        if ($foreign.isUndefined(value)) {
            return Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(dictMonad))(Data_Maybe.Nothing.value);
        };
        if (Data_Boolean.otherwise) {
            return Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(dictMonad))(new Data_Maybe.Just(value));
        };
        throw new Error("Failed pattern match at Foreign (line 161, column 1 - line 161, column 70): " + [ value.constructor.name ]);
    };
};
var readNullOrUndefined = function (dictMonad) {
    return function (value) {
        if ($foreign.isNull(value) || $foreign.isUndefined(value)) {
            return Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(dictMonad))(Data_Maybe.Nothing.value);
        };
        if (Data_Boolean.otherwise) {
            return Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(dictMonad))(new Data_Maybe.Just(value));
        };
        throw new Error("Failed pattern match at Foreign (line 166, column 1 - line 166, column 76): " + [ value.constructor.name ]);
    };
};
var readNull = function (dictMonad) {
    return function (value) {
        if ($foreign.isNull(value)) {
            return Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(dictMonad))(Data_Maybe.Nothing.value);
        };
        if (Data_Boolean.otherwise) {
            return Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(dictMonad))(new Data_Maybe.Just(value));
        };
        throw new Error("Failed pattern match at Foreign (line 156, column 1 - line 156, column 65): " + [ value.constructor.name ]);
    };
};
var fail = function (dictMonad) {
    var $118 = Control_Monad_Error_Class.throwError(Control_Monad_Except_Trans.monadThrowExceptT(dictMonad));
    return function ($119) {
        return $118(Data_List_NonEmpty.singleton($119));
    };
};
var readArray = function (dictMonad) {
    return function (value) {
        if ($foreign.isArray(value)) {
            return Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(dictMonad))(unsafeFromForeign(value));
        };
        if (Data_Boolean.otherwise) {
            return fail(dictMonad)(new TypeMismatch("array", $foreign.tagOf(value)));
        };
        throw new Error("Failed pattern match at Foreign (line 151, column 1 - line 151, column 66): " + [ value.constructor.name ]);
    };
};
var unsafeReadTagged = function (dictMonad) {
    return function (tag) {
        return function (value) {
            if ($foreign.tagOf(value) === tag) {
                return Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(dictMonad))(unsafeFromForeign(value));
            };
            if (Data_Boolean.otherwise) {
                return fail(dictMonad)(new TypeMismatch(tag, $foreign.tagOf(value)));
            };
            throw new Error("Failed pattern match at Foreign (line 110, column 1 - line 110, column 71): " + [ tag.constructor.name, value.constructor.name ]);
        };
    };
};
var readBoolean = function (dictMonad) {
    return unsafeReadTagged(dictMonad)("Boolean");
};
var readNumber = function (dictMonad) {
    return unsafeReadTagged(dictMonad)("Number");
};
var readInt = function (dictMonad) {
    return function (value) {
        var error = new Data_Either.Left(Data_List_NonEmpty.singleton(new TypeMismatch("Int", $foreign.tagOf(value))));
        var fromNumber = (function () {
            var $120 = Data_Maybe.maybe(error)(Control_Applicative.pure(Data_Either.applicativeEither));
            return function ($121) {
                return $120(Data_Int.fromNumber($121));
            };
        })();
        return Control_Monad_Except_Trans.mapExceptT(Data_Functor.map(((dictMonad.Bind1()).Apply0()).Functor0())(Data_Either.either(Data_Function["const"](error))(fromNumber)))(readNumber(dictMonad)(value));
    };
};
var readString = function (dictMonad) {
    return unsafeReadTagged(dictMonad)("String");
};
var readChar = function (dictMonad) {
    return function (value) {
        var error = new Data_Either.Left(Data_List_NonEmpty.singleton(new TypeMismatch("Char", $foreign.tagOf(value))));
        var fromString = (function () {
            var $122 = Data_Maybe.maybe(error)(Control_Applicative.pure(Data_Either.applicativeEither));
            return function ($123) {
                return $122(Data_String_CodeUnits.toChar($123));
            };
        })();
        return Control_Monad_Except_Trans.mapExceptT(Data_Functor.map(((dictMonad.Bind1()).Apply0()).Functor0())(Data_Either.either(Data_Function["const"](error))(fromString)))(readString(dictMonad)(value));
    };
};
var eqForeignError = {
    eq: function (x) {
        return function (y) {
            if (x instanceof ForeignError && y instanceof ForeignError) {
                return x.value0 === y.value0;
            };
            if (x instanceof TypeMismatch && y instanceof TypeMismatch) {
                return x.value0 === y.value0 && x.value1 === y.value1;
            };
            if (x instanceof ErrorAtIndex && y instanceof ErrorAtIndex) {
                return x.value0 === y.value0 && Data_Eq.eq(eqForeignError)(x.value1)(y.value1);
            };
            if (x instanceof ErrorAtProperty && y instanceof ErrorAtProperty) {
                return x.value0 === y.value0 && Data_Eq.eq(eqForeignError)(x.value1)(y.value1);
            };
            return false;
        };
    }
};
var ordForeignError = {
    compare: function (x) {
        return function (y) {
            if (x instanceof ForeignError && y instanceof ForeignError) {
                return Data_Ord.compare(Data_Ord.ordString)(x.value0)(y.value0);
            };
            if (x instanceof ForeignError) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof ForeignError) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof TypeMismatch && y instanceof TypeMismatch) {
                var v = Data_Ord.compare(Data_Ord.ordString)(x.value0)(y.value0);
                if (v instanceof Data_Ordering.LT) {
                    return Data_Ordering.LT.value;
                };
                if (v instanceof Data_Ordering.GT) {
                    return Data_Ordering.GT.value;
                };
                return Data_Ord.compare(Data_Ord.ordString)(x.value1)(y.value1);
            };
            if (x instanceof TypeMismatch) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof TypeMismatch) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof ErrorAtIndex && y instanceof ErrorAtIndex) {
                var v = Data_Ord.compare(Data_Ord.ordInt)(x.value0)(y.value0);
                if (v instanceof Data_Ordering.LT) {
                    return Data_Ordering.LT.value;
                };
                if (v instanceof Data_Ordering.GT) {
                    return Data_Ordering.GT.value;
                };
                return Data_Ord.compare(ordForeignError)(x.value1)(y.value1);
            };
            if (x instanceof ErrorAtIndex) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof ErrorAtIndex) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof ErrorAtProperty && y instanceof ErrorAtProperty) {
                var v = Data_Ord.compare(Data_Ord.ordString)(x.value0)(y.value0);
                if (v instanceof Data_Ordering.LT) {
                    return Data_Ordering.LT.value;
                };
                if (v instanceof Data_Ordering.GT) {
                    return Data_Ordering.GT.value;
                };
                return Data_Ord.compare(ordForeignError)(x.value1)(y.value1);
            };
            throw new Error("Failed pattern match at Foreign (line 62, column 1 - line 62, column 52): " + [ x.constructor.name, y.constructor.name ]);
        };
    },
    Eq0: function () {
        return eqForeignError;
    }
};
module.exports = {
    ForeignError: ForeignError,
    TypeMismatch: TypeMismatch,
    ErrorAtIndex: ErrorAtIndex,
    ErrorAtProperty: ErrorAtProperty,
    renderForeignError: renderForeignError,
    unsafeToForeign: unsafeToForeign,
    unsafeFromForeign: unsafeFromForeign,
    unsafeReadTagged: unsafeReadTagged,
    readString: readString,
    readChar: readChar,
    readBoolean: readBoolean,
    readNumber: readNumber,
    readInt: readInt,
    readArray: readArray,
    readNull: readNull,
    readUndefined: readUndefined,
    readNullOrUndefined: readNullOrUndefined,
    fail: fail,
    eqForeignError: eqForeignError,
    ordForeignError: ordForeignError,
    showForeignError: showForeignError,
    typeOf: $foreign.typeOf,
    tagOf: $foreign.tagOf,
    isNull: $foreign.isNull,
    isUndefined: $foreign.isUndefined,
    isArray: $foreign.isArray
};
