// Generated by purs version 0.14.7
"use strict";
var $foreign = require("./foreign.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Nullable = require("../Data.Nullable/index.js");
var Unsafe_Coerce = require("../Unsafe.Coerce/index.js");
var Web_Internal_FFI = require("../Web.Internal.FFI/index.js");
var toUIEvent = Unsafe_Coerce.unsafeCoerce;
var toEvent = Unsafe_Coerce.unsafeCoerce;
var relatedTarget = Data_Functor.map(Data_Functor.functorFn)(Data_Nullable.toMaybe)($foreign["_relatedTarget"]);
var fromUIEvent = Web_Internal_FFI.unsafeReadProtoTagged("FocusEvent");
var fromEvent = Web_Internal_FFI.unsafeReadProtoTagged("FocusEvent");
module.exports = {
    fromUIEvent: fromUIEvent,
    fromEvent: fromEvent,
    toUIEvent: toUIEvent,
    toEvent: toEvent,
    relatedTarget: relatedTarget
};
