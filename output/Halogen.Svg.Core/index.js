// Generated by purs version 0.14.7
"use strict";
var Data_Maybe = require("../Data.Maybe/index.js");
var Halogen_VDom_DOM_Prop = require("../Halogen.VDom.DOM.Prop/index.js");
var Halogen_VDom_Types = require("../Halogen.VDom.Types/index.js");
var ns = new Data_Maybe.Just("http://www.w3.org/2000/svg");
var element = function (name) {
    return function (props) {
        return function (children) {
            return new Halogen_VDom_Types.Elem(ns, name, props, children);
        };
    };
};
var attr = function (v) {
    return Halogen_VDom_DOM_Prop.Attribute.create(Data_Maybe.Nothing.value)(v);
};
module.exports = {
    ns: ns,
    element: element,
    attr: attr
};
