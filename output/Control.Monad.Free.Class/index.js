// Generated by purs version 0.14.7
"use strict";
var Control_Bind = require("../Control.Bind/index.js");
var Control_Monad_Except_Trans = require("../Control.Monad.Except.Trans/index.js");
var Control_Monad_Free = require("../Control.Monad.Free/index.js");
var Control_Monad_Maybe_Trans = require("../Control.Monad.Maybe.Trans/index.js");
var Control_Monad_Reader_Trans = require("../Control.Monad.Reader.Trans/index.js");
var Control_Monad_State_Trans = require("../Control.Monad.State.Trans/index.js");
var Control_Monad_Writer_Trans = require("../Control.Monad.Writer.Trans/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var wrapFree = function (dict) {
    return dict.wrapFree;
};
var monadFreeWriterT = function (dictFunctor) {
    return function (dictMonadFree) {
        return function (dictMonoid) {
            return {
                wrapFree: function (f) {
                    return wrapFree(dictMonadFree)(Data_Functor.map(dictFunctor)(Control_Monad_Writer_Trans.runWriterT)(f));
                },
                Monad0: function () {
                    return Control_Monad_Writer_Trans.monadWriterT(dictMonoid)(dictMonadFree.Monad0());
                }
            };
        };
    };
};
var monadFreeStateT = function (dictFunctor) {
    return function (dictMonadFree) {
        return {
            wrapFree: function (f) {
                return function (s) {
                    return wrapFree(dictMonadFree)(Data_Functor.map(dictFunctor)(function (st) {
                        return Control_Monad_State_Trans.runStateT(st)(s);
                    })(f));
                };
            },
            Monad0: function () {
                return Control_Monad_State_Trans.monadStateT(dictMonadFree.Monad0());
            }
        };
    };
};
var monadFreeReaderT = function (dictFunctor) {
    return function (dictMonadFree) {
        return {
            wrapFree: function (f) {
                return function (r) {
                    return wrapFree(dictMonadFree)(Data_Functor.map(dictFunctor)(function (rt) {
                        return Control_Monad_Reader_Trans.runReaderT(rt)(r);
                    })(f));
                };
            },
            Monad0: function () {
                return Control_Monad_Reader_Trans.monadReaderT(dictMonadFree.Monad0());
            }
        };
    };
};
var monadFreeMaybeT = function (dictFunctor) {
    return function (dictMonadFree) {
        return {
            wrapFree: function (f) {
                return wrapFree(dictMonadFree)(Data_Functor.map(dictFunctor)(Control_Monad_Maybe_Trans.runMaybeT)(f));
            },
            Monad0: function () {
                return Control_Monad_Maybe_Trans.monadMaybeT(dictMonadFree.Monad0());
            }
        };
    };
};
var monadFreeFree = {
    wrapFree: (function () {
        var $12 = Control_Bind.join(Control_Monad_Free.freeBind);
        return function ($13) {
            return $12(Control_Monad_Free.liftF($13));
        };
    })(),
    Monad0: function () {
        return Control_Monad_Free.freeMonad;
    }
};
var monadFreeExceptT = function (dictFunctor) {
    return function (dictMonadFree) {
        return {
            wrapFree: function (f) {
                return wrapFree(dictMonadFree)(Data_Functor.map(dictFunctor)(Control_Monad_Except_Trans.runExceptT)(f));
            },
            Monad0: function () {
                return Control_Monad_Except_Trans.monadExceptT(dictMonadFree.Monad0());
            }
        };
    };
};
module.exports = {
    wrapFree: wrapFree,
    monadFreeFree: monadFreeFree,
    monadFreeReaderT: monadFreeReaderT,
    monadFreeStateT: monadFreeStateT,
    monadFreeWriterT: monadFreeWriterT,
    monadFreeMaybeT: monadFreeMaybeT,
    monadFreeExceptT: monadFreeExceptT
};
