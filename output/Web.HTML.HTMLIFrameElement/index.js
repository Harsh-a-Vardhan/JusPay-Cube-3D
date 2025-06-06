// Generated by purs version 0.14.7
"use strict";
var $foreign = require("./foreign.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Nullable = require("../Data.Nullable/index.js");
var Effect = require("../Effect/index.js");
var Unsafe_Coerce = require("../Unsafe.Coerce/index.js");
var Web_Internal_FFI = require("../Web.Internal.FFI/index.js");
var toParentNode = Unsafe_Coerce.unsafeCoerce;
var toNonDocumentTypeChildNode = Unsafe_Coerce.unsafeCoerce;
var toNode = Unsafe_Coerce.unsafeCoerce;
var toHTMLElement = Unsafe_Coerce.unsafeCoerce;
var toEventTarget = Unsafe_Coerce.unsafeCoerce;
var toElement = Unsafe_Coerce.unsafeCoerce;
var toChildNode = Unsafe_Coerce.unsafeCoerce;
var fromParentNode = Web_Internal_FFI.unsafeReadProtoTagged("HTMLIFrameElement");
var fromNonDocumentTypeChildNode = Web_Internal_FFI.unsafeReadProtoTagged("HTMLIFrameElement");
var fromNode = Web_Internal_FFI.unsafeReadProtoTagged("HTMLIFrameElement");
var fromHTMLElement = Web_Internal_FFI.unsafeReadProtoTagged("HTMLIFrameElement");
var fromEventTarget = Web_Internal_FFI.unsafeReadProtoTagged("HTMLIFrameElement");
var fromElement = Web_Internal_FFI.unsafeReadProtoTagged("HTMLIFrameElement");
var fromChildNode = Web_Internal_FFI.unsafeReadProtoTagged("HTMLIFrameElement");
var contentWindow = (function () {
    var $0 = Data_Functor.map(Effect.functorEffect)(Data_Nullable.toMaybe);
    return function ($1) {
        return $0($foreign["_contentWindow"]($1));
    };
})();
var contentDocument = (function () {
    var $2 = Data_Functor.map(Effect.functorEffect)(Data_Nullable.toMaybe);
    return function ($3) {
        return $2($foreign["_contentDocument"]($3));
    };
})();
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
    contentDocument: contentDocument,
    contentWindow: contentWindow,
    src: $foreign.src,
    setSrc: $foreign.setSrc,
    srcdoc: $foreign.srcdoc,
    setSrcdoc: $foreign.setSrcdoc,
    name: $foreign.name,
    setName: $foreign.setName,
    width: $foreign.width,
    setWidth: $foreign.setWidth,
    height: $foreign.height,
    setHeight: $foreign.setHeight
};
