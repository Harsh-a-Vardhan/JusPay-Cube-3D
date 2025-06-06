// Generated by purs version 0.14.7
"use strict";
var $foreign = require("./foreign.js");
var Unsafe_Coerce = require("../Unsafe.Coerce/index.js");
var Web_Internal_FFI = require("../Web.Internal.FFI/index.js");
var toParentNode = Unsafe_Coerce.unsafeCoerce;
var toNonDocumentTypeChildNode = Unsafe_Coerce.unsafeCoerce;
var toNode = Unsafe_Coerce.unsafeCoerce;
var toHTMLElement = Unsafe_Coerce.unsafeCoerce;
var toEventTarget = Unsafe_Coerce.unsafeCoerce;
var toElement = Unsafe_Coerce.unsafeCoerce;
var toChildNode = Unsafe_Coerce.unsafeCoerce;
var fromParentNode = Web_Internal_FFI.unsafeReadProtoTagged("HTMLScriptElement");
var fromNonDocumentTypeChildNode = Web_Internal_FFI.unsafeReadProtoTagged("HTMLScriptElement");
var fromNode = Web_Internal_FFI.unsafeReadProtoTagged("HTMLScriptElement");
var fromHTMLElement = Web_Internal_FFI.unsafeReadProtoTagged("HTMLScriptElement");
var fromEventTarget = Web_Internal_FFI.unsafeReadProtoTagged("HTMLScriptElement");
var fromElement = Web_Internal_FFI.unsafeReadProtoTagged("HTMLScriptElement");
var fromChildNode = Web_Internal_FFI.unsafeReadProtoTagged("HTMLScriptElement");
module.exports = {
    fromHTMLElement: fromHTMLElement,
    fromElement: fromElement,
    fromNode: fromNode,
    fromChildNode: fromChildNode,
    fromNonDocumentTypeChildNode: fromNonDocumentTypeChildNode,
    fromParentNode: fromParentNode,
    fromEventTarget: fromEventTarget,
    toHTMLElement: toHTMLElement,
    toElement: toElement,
    toNode: toNode,
    toChildNode: toChildNode,
    toNonDocumentTypeChildNode: toNonDocumentTypeChildNode,
    toParentNode: toParentNode,
    toEventTarget: toEventTarget,
    src: $foreign.src,
    setSrc: $foreign.setSrc,
    type_: $foreign.type_,
    setType: $foreign.setType,
    charset: $foreign.charset,
    setCharset: $foreign.setCharset,
    async: $foreign.async,
    setAsync: $foreign.setAsync,
    defer: $foreign.defer,
    setDefer: $foreign.setDefer,
    crossOrigin: $foreign.crossOrigin,
    setCrossOrigin: $foreign.setCrossOrigin,
    text: $foreign.text,
    setText: $foreign.setText
};
