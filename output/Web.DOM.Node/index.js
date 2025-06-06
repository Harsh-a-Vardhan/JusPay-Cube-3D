// Generated by purs version 0.14.7
"use strict";
var $foreign = require("./foreign.js");
var Data_Enum = require("../Data.Enum/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Nullable = require("../Data.Nullable/index.js");
var Effect = require("../Effect/index.js");
var Unsafe_Coerce = require("../Unsafe.Coerce/index.js");
var Web_DOM_Internal_Types = require("../Web.DOM.Internal.Types/index.js");
var Web_DOM_NodeType = require("../Web.DOM.NodeType/index.js");
var Web_Internal_FFI = require("../Web.Internal.FFI/index.js");
var toEventTarget = Unsafe_Coerce.unsafeCoerce;
var previousSibling = (function () {
    var $0 = Data_Functor.map(Effect.functorEffect)(Data_Nullable.toMaybe);
    return function ($1) {
        return $0($foreign["_previousSibling"]($1));
    };
})();
var parentNode = (function () {
    var $2 = Data_Functor.map(Effect.functorEffect)(Data_Nullable.toMaybe);
    return function ($3) {
        return $2($foreign["_parentNode"]($3));
    };
})();
var parentElement = (function () {
    var $4 = Data_Functor.map(Effect.functorEffect)(Data_Nullable.toMaybe);
    return function ($5) {
        return $4($foreign["_parentElement"]($5));
    };
})();
var ownerDocument = (function () {
    var $6 = Data_Functor.map(Effect.functorEffect)(Data_Nullable.toMaybe);
    return function ($7) {
        return $6($foreign["_ownerDocument"]($7));
    };
})();
var nodeValue = (function () {
    var $8 = Data_Functor.map(Effect.functorEffect)(Data_Nullable.toMaybe);
    return function ($9) {
        return $8($foreign["_nodeValue"]($9));
    };
})();
var nodeType = function () {
    var $10 = Data_Maybe.fromJust();
    var $11 = Data_Enum.toEnum(Web_DOM_NodeType.boundedEnumNodeType);
    return function ($12) {
        return $10($11($foreign.nodeTypeIndex($12)));
    };
};
var nextSibling = (function () {
    var $13 = Data_Functor.map(Effect.functorEffect)(Data_Nullable.toMaybe);
    return function ($14) {
        return $13($foreign["_nextSibling"]($14));
    };
})();
var lookupPrefix = function (p) {
    var $15 = Data_Functor.map(Effect.functorEffect)(Data_Nullable.toMaybe);
    var $16 = $foreign["_lookupPrefix"](p);
    return function ($17) {
        return $15($16($17));
    };
};
var lookupNamespaceURI = function (ns) {
    var $18 = Data_Functor.map(Effect.functorEffect)(Data_Nullable.toMaybe);
    var $19 = $foreign["_lookupNamespaceURI"](ns);
    return function ($20) {
        return $18($19($20));
    };
};
var lastChild = (function () {
    var $21 = Data_Functor.map(Effect.functorEffect)(Data_Nullable.toMaybe);
    return function ($22) {
        return $21($foreign["_lastChild"]($22));
    };
})();
var fromEventTarget = Web_Internal_FFI.unsafeReadProtoTagged("Node");
var firstChild = (function () {
    var $23 = Data_Functor.map(Effect.functorEffect)(Data_Nullable.toMaybe);
    return function ($24) {
        return $23($foreign["_firstChild"]($24));
    };
})();
module.exports = {
    fromEventTarget: fromEventTarget,
    toEventTarget: toEventTarget,
    nodeType: nodeType,
    ownerDocument: ownerDocument,
    parentNode: parentNode,
    parentElement: parentElement,
    firstChild: firstChild,
    lastChild: lastChild,
    previousSibling: previousSibling,
    nextSibling: nextSibling,
    nodeValue: nodeValue,
    lookupPrefix: lookupPrefix,
    lookupNamespaceURI: lookupNamespaceURI,
    nodeTypeIndex: $foreign.nodeTypeIndex,
    nodeName: $foreign.nodeName,
    baseURI: $foreign.baseURI,
    hasChildNodes: $foreign.hasChildNodes,
    childNodes: $foreign.childNodes,
    setNodeValue: $foreign.setNodeValue,
    textContent: $foreign.textContent,
    setTextContent: $foreign.setTextContent,
    normalize: $foreign.normalize,
    clone: $foreign.clone,
    deepClone: $foreign.deepClone,
    isEqualNode: $foreign.isEqualNode,
    compareDocumentPositionBits: $foreign.compareDocumentPositionBits,
    contains: $foreign.contains,
    isDefaultNamespace: $foreign.isDefaultNamespace,
    insertBefore: $foreign.insertBefore,
    appendChild: $foreign.appendChild,
    replaceChild: $foreign.replaceChild,
    removeChild: $foreign.removeChild
};
