// Generated by purs version 0.14.7
"use strict";
var Control_Monad_Reader_Class = require("../Control.Monad.Reader.Class/index.js");
var Control_Monad_Reader_Trans = require("../Control.Monad.Reader.Trans/index.js");
var Data_Identity = require("../Data.Identity/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var withReader = Control_Monad_Reader_Trans.withReaderT;
var runReader = function (v) {
    var $2 = Data_Newtype.unwrap();
    return function ($3) {
        return $2(v($3));
    };
};
var mapReader = function (f) {
    return Control_Monad_Reader_Trans.mapReaderT((function () {
        var $4 = Data_Newtype.unwrap();
        return function ($5) {
            return Data_Identity.Identity(f($4($5)));
        };
    })());
};
module.exports = {
    runReader: runReader,
    mapReader: mapReader,
    withReader: withReader,
    ask: Control_Monad_Reader_Class.ask,
    asks: Control_Monad_Reader_Class.asks,
    local: Control_Monad_Reader_Class.local,
    ReaderT: Control_Monad_Reader_Trans.ReaderT,
    lift: Control_Monad_Reader_Trans.lift,
    mapReaderT: Control_Monad_Reader_Trans.mapReaderT,
    runReaderT: Control_Monad_Reader_Trans.runReaderT,
    withReaderT: Control_Monad_Reader_Trans.withReaderT
};
