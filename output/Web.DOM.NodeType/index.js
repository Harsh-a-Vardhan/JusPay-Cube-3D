// Generated by purs version 0.14.7
"use strict";
var Data_Enum = require("../Data.Enum/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var ElementNode = (function () {
    function ElementNode() {

    };
    ElementNode.value = new ElementNode();
    return ElementNode;
})();
var AttributeNode = (function () {
    function AttributeNode() {

    };
    AttributeNode.value = new AttributeNode();
    return AttributeNode;
})();
var TextNode = (function () {
    function TextNode() {

    };
    TextNode.value = new TextNode();
    return TextNode;
})();
var CDATASectionNode = (function () {
    function CDATASectionNode() {

    };
    CDATASectionNode.value = new CDATASectionNode();
    return CDATASectionNode;
})();
var EntityReferenceNode = (function () {
    function EntityReferenceNode() {

    };
    EntityReferenceNode.value = new EntityReferenceNode();
    return EntityReferenceNode;
})();
var EntityNode = (function () {
    function EntityNode() {

    };
    EntityNode.value = new EntityNode();
    return EntityNode;
})();
var ProcessingInstructionNode = (function () {
    function ProcessingInstructionNode() {

    };
    ProcessingInstructionNode.value = new ProcessingInstructionNode();
    return ProcessingInstructionNode;
})();
var CommentNode = (function () {
    function CommentNode() {

    };
    CommentNode.value = new CommentNode();
    return CommentNode;
})();
var DocumentNode = (function () {
    function DocumentNode() {

    };
    DocumentNode.value = new DocumentNode();
    return DocumentNode;
})();
var DocumentTypeNode = (function () {
    function DocumentTypeNode() {

    };
    DocumentTypeNode.value = new DocumentTypeNode();
    return DocumentTypeNode;
})();
var DocumentFragmentNode = (function () {
    function DocumentFragmentNode() {

    };
    DocumentFragmentNode.value = new DocumentFragmentNode();
    return DocumentFragmentNode;
})();
var NotationNode = (function () {
    function NotationNode() {

    };
    NotationNode.value = new NotationNode();
    return NotationNode;
})();
var toEnumNodeType = function (v) {
    if (v === 1) {
        return new Data_Maybe.Just(ElementNode.value);
    };
    if (v === 2) {
        return new Data_Maybe.Just(AttributeNode.value);
    };
    if (v === 3) {
        return new Data_Maybe.Just(TextNode.value);
    };
    if (v === 4) {
        return new Data_Maybe.Just(CDATASectionNode.value);
    };
    if (v === 5) {
        return new Data_Maybe.Just(EntityReferenceNode.value);
    };
    if (v === 6) {
        return new Data_Maybe.Just(EntityNode.value);
    };
    if (v === 7) {
        return new Data_Maybe.Just(ProcessingInstructionNode.value);
    };
    if (v === 8) {
        return new Data_Maybe.Just(CommentNode.value);
    };
    if (v === 9) {
        return new Data_Maybe.Just(DocumentNode.value);
    };
    if (v === 10) {
        return new Data_Maybe.Just(DocumentTypeNode.value);
    };
    if (v === 11) {
        return new Data_Maybe.Just(DocumentFragmentNode.value);
    };
    if (v === 12) {
        return new Data_Maybe.Just(NotationNode.value);
    };
    return Data_Maybe.Nothing.value;
};
var showNodeType = {
    show: function (v) {
        if (v instanceof ElementNode) {
            return "ElementNode";
        };
        if (v instanceof AttributeNode) {
            return "AttributeNode";
        };
        if (v instanceof TextNode) {
            return "TextNode";
        };
        if (v instanceof CDATASectionNode) {
            return "CDATASectionNode";
        };
        if (v instanceof EntityReferenceNode) {
            return "EntityReferenceNode";
        };
        if (v instanceof EntityNode) {
            return "EntityNode";
        };
        if (v instanceof ProcessingInstructionNode) {
            return "ProcessingInstructionNode";
        };
        if (v instanceof CommentNode) {
            return "CommentNode";
        };
        if (v instanceof DocumentNode) {
            return "DocumentNode";
        };
        if (v instanceof DocumentTypeNode) {
            return "DocumentTypeNode";
        };
        if (v instanceof DocumentFragmentNode) {
            return "DocumentFragmentNode";
        };
        if (v instanceof NotationNode) {
            return "NotationNode";
        };
        throw new Error("Failed pattern match at Web.DOM.NodeType (line 39, column 1 - line 51, column 37): " + [ v.constructor.name ]);
    }
};
var fromEnumNodeType = function (v) {
    if (v instanceof ElementNode) {
        return 1;
    };
    if (v instanceof AttributeNode) {
        return 2;
    };
    if (v instanceof TextNode) {
        return 3;
    };
    if (v instanceof CDATASectionNode) {
        return 4;
    };
    if (v instanceof EntityReferenceNode) {
        return 5;
    };
    if (v instanceof EntityNode) {
        return 6;
    };
    if (v instanceof ProcessingInstructionNode) {
        return 7;
    };
    if (v instanceof CommentNode) {
        return 8;
    };
    if (v instanceof DocumentNode) {
        return 9;
    };
    if (v instanceof DocumentTypeNode) {
        return 10;
    };
    if (v instanceof DocumentFragmentNode) {
        return 11;
    };
    if (v instanceof NotationNode) {
        return 12;
    };
    throw new Error("Failed pattern match at Web.DOM.NodeType (line 68, column 1 - line 68, column 36): " + [ v.constructor.name ]);
};
var eqNodeType = {
    eq: function (x) {
        return function (y) {
            if (x instanceof ElementNode && y instanceof ElementNode) {
                return true;
            };
            if (x instanceof AttributeNode && y instanceof AttributeNode) {
                return true;
            };
            if (x instanceof TextNode && y instanceof TextNode) {
                return true;
            };
            if (x instanceof CDATASectionNode && y instanceof CDATASectionNode) {
                return true;
            };
            if (x instanceof EntityReferenceNode && y instanceof EntityReferenceNode) {
                return true;
            };
            if (x instanceof EntityNode && y instanceof EntityNode) {
                return true;
            };
            if (x instanceof ProcessingInstructionNode && y instanceof ProcessingInstructionNode) {
                return true;
            };
            if (x instanceof CommentNode && y instanceof CommentNode) {
                return true;
            };
            if (x instanceof DocumentNode && y instanceof DocumentNode) {
                return true;
            };
            if (x instanceof DocumentTypeNode && y instanceof DocumentTypeNode) {
                return true;
            };
            if (x instanceof DocumentFragmentNode && y instanceof DocumentFragmentNode) {
                return true;
            };
            if (x instanceof NotationNode && y instanceof NotationNode) {
                return true;
            };
            return false;
        };
    }
};
var ordNodeType = {
    compare: function (x) {
        return function (y) {
            return Data_Ord.compare(Data_Ord.ordInt)(fromEnumNodeType(x))(fromEnumNodeType(y));
        };
    },
    Eq0: function () {
        return eqNodeType;
    }
};
var enumNodeType = {
    succ: Data_Enum.defaultSucc(toEnumNodeType)(fromEnumNodeType),
    pred: Data_Enum.defaultPred(toEnumNodeType)(fromEnumNodeType),
    Ord0: function () {
        return ordNodeType;
    }
};
var boundedNodeType = {
    bottom: ElementNode.value,
    top: NotationNode.value,
    Ord0: function () {
        return ordNodeType;
    }
};
var boundedEnumNodeType = {
    cardinality: 12,
    toEnum: toEnumNodeType,
    fromEnum: fromEnumNodeType,
    Bounded0: function () {
        return boundedNodeType;
    },
    Enum1: function () {
        return enumNodeType;
    }
};
module.exports = {
    ElementNode: ElementNode,
    AttributeNode: AttributeNode,
    TextNode: TextNode,
    CDATASectionNode: CDATASectionNode,
    EntityReferenceNode: EntityReferenceNode,
    EntityNode: EntityNode,
    ProcessingInstructionNode: ProcessingInstructionNode,
    CommentNode: CommentNode,
    DocumentNode: DocumentNode,
    DocumentTypeNode: DocumentTypeNode,
    DocumentFragmentNode: DocumentFragmentNode,
    NotationNode: NotationNode,
    eqNodeType: eqNodeType,
    ordNodeType: ordNodeType,
    boundedNodeType: boundedNodeType,
    enumNodeType: enumNodeType,
    boundedEnumNodeType: boundedEnumNodeType,
    showNodeType: showNodeType
};
