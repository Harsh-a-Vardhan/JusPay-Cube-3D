// Generated by purs version 0.14.7
"use strict";
var Data_Functor = require("../Data.Functor/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Nullable = require("../Data.Nullable/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Effect_Ref = require("../Effect.Ref/index.js");
var Foreign = require("../Foreign/index.js");
var Foreign_Object = require("../Foreign.Object/index.js");
var Halogen_VDom_Machine = require("../Halogen.VDom.Machine/index.js");
var Halogen_VDom_Util = require("../Halogen.VDom.Util/index.js");
var Unsafe_Coerce = require("../Unsafe.Coerce/index.js");
var Web_Event_EventTarget = require("../Web.Event.EventTarget/index.js");
var Created = (function () {
    function Created(value0) {
        this.value0 = value0;
    };
    Created.create = function (value0) {
        return new Created(value0);
    };
    return Created;
})();
var Removed = (function () {
    function Removed(value0) {
        this.value0 = value0;
    };
    Removed.create = function (value0) {
        return new Removed(value0);
    };
    return Removed;
})();
var Attribute = (function () {
    function Attribute(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    Attribute.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new Attribute(value0, value1, value2);
            };
        };
    };
    return Attribute;
})();
var Property = (function () {
    function Property(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Property.create = function (value0) {
        return function (value1) {
            return new Property(value0, value1);
        };
    };
    return Property;
})();
var Handler = (function () {
    function Handler(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Handler.create = function (value0) {
        return function (value1) {
            return new Handler(value0, value1);
        };
    };
    return Handler;
})();
var Ref = (function () {
    function Ref(value0) {
        this.value0 = value0;
    };
    Ref.create = function (value0) {
        return new Ref(value0);
    };
    return Ref;
})();
var unsafeGetProperty = Halogen_VDom_Util.unsafeGetAny;
var setProperty = Halogen_VDom_Util.unsafeSetAny;
var removeProperty = function (key, el) {
    var v = Halogen_VDom_Util.hasAttribute(Data_Nullable["null"], key, el);
    if (v) {
        return Halogen_VDom_Util.removeAttribute(Data_Nullable["null"], key, el);
    };
    var v1 = Foreign.typeOf(Halogen_VDom_Util.unsafeGetAny(key, el));
    if (v1 === "string") {
        return Halogen_VDom_Util.unsafeSetAny(key, "", el);
    };
    if (key === "rowSpan") {
        return Halogen_VDom_Util.unsafeSetAny(key, 1, el);
    };
    if (key === "colSpan") {
        return Halogen_VDom_Util.unsafeSetAny(key, 1, el);
    };
    return Halogen_VDom_Util.unsafeSetAny(key, Halogen_VDom_Util.jsUndefined, el);
};
var propToStrKey = function (v) {
    if (v instanceof Attribute && v.value0 instanceof Data_Maybe.Just) {
        return "attr/" + (v.value0.value0 + (":" + v.value1));
    };
    if (v instanceof Attribute) {
        return "attr/:" + v.value1;
    };
    if (v instanceof Property) {
        return "prop/" + v.value0;
    };
    if (v instanceof Handler) {
        return "handler/" + v.value0;
    };
    if (v instanceof Ref) {
        return "ref";
    };
    throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 182, column 16 - line 187, column 16): " + [ v.constructor.name ]);
};
var propFromString = Unsafe_Coerce.unsafeCoerce;
var propFromNumber = Unsafe_Coerce.unsafeCoerce;
var propFromInt = Unsafe_Coerce.unsafeCoerce;
var propFromBoolean = Unsafe_Coerce.unsafeCoerce;
var functorProp = {
    map: function (f) {
        return function (v) {
            if (v instanceof Handler) {
                return new Handler(v.value0, Data_Functor.map(Data_Functor.functorFn)(Data_Functor.map(Data_Maybe.functorMaybe)(f))(v.value1));
            };
            if (v instanceof Ref) {
                return new Ref(Data_Functor.map(Data_Functor.functorFn)(Data_Functor.map(Data_Maybe.functorMaybe)(f))(v.value0));
            };
            return v;
        };
    }
};
var functorElemRef = {
    map: function (f) {
        return function (v) {
            if (v instanceof Created) {
                return new Created(f(v.value0));
            };
            if (v instanceof Removed) {
                return new Removed(f(v.value0));
            };
            throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 49, column 1 - line 51, column 36): " + [ f.constructor.name, v.constructor.name ]);
        };
    }
};
var buildProp = function (emit) {
    return function (el) {
        var removeProp = function (prevEvents) {
            return function (v, v1) {
                if (v1 instanceof Attribute) {
                    return Halogen_VDom_Util.removeAttribute(Data_Nullable.toNullable(v1.value0), v1.value1, el);
                };
                if (v1 instanceof Property) {
                    return removeProperty(v1.value0, el);
                };
                if (v1 instanceof Handler) {
                    var handler = Halogen_VDom_Util.unsafeLookup(v1.value0, prevEvents);
                    return Halogen_VDom_Util.removeEventListener(v1.value0, Data_Tuple.fst(handler), el);
                };
                if (v1 instanceof Ref) {
                    return Data_Unit.unit;
                };
                throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 169, column 5 - line 179, column 18): " + [ v1.constructor.name ]);
            };
        };
        var mbEmit = function (v) {
            if (v instanceof Data_Maybe.Just) {
                return emit(v.value0)();
            };
            return Data_Unit.unit;
        };
        var haltProp = function (state) {
            var v = Foreign_Object.lookup("ref")(state.props);
            if (v instanceof Data_Maybe.Just && v.value0 instanceof Ref) {
                return mbEmit(v.value0.value0(new Removed(el)));
            };
            return Data_Unit.unit;
        };
        var diffProp = function (prevEvents, events) {
            return function (v, v1, v11, v2) {
                if (v11 instanceof Attribute && v2 instanceof Attribute) {
                    var $56 = v11.value2 === v2.value2;
                    if ($56) {
                        return v2;
                    };
                    Halogen_VDom_Util.setAttribute(Data_Nullable.toNullable(v2.value0), v2.value1, v2.value2, el);
                    return v2;
                };
                if (v11 instanceof Property && v2 instanceof Property) {
                    var v4 = Halogen_VDom_Util.refEq(v11.value1, v2.value1);
                    if (v4) {
                        return v2;
                    };
                    if (v2.value0 === "value") {
                        var elVal = unsafeGetProperty("value", el);
                        var $65 = Halogen_VDom_Util.refEq(elVal, v2.value1);
                        if ($65) {
                            return v2;
                        };
                        setProperty(v2.value0, v2.value1, el);
                        return v2;
                    };
                    setProperty(v2.value0, v2.value1, el);
                    return v2;
                };
                if (v11 instanceof Handler && v2 instanceof Handler) {
                    var handler = Halogen_VDom_Util.unsafeLookup(v2.value0, prevEvents);
                    Effect_Ref.write(v2.value1)(Data_Tuple.snd(handler))();
                    Halogen_VDom_Util.pokeMutMap(v2.value0, handler, events);
                    return v2;
                };
                return v2;
            };
        };
        var applyProp = function (events) {
            return function (v, v1, v2) {
                if (v2 instanceof Attribute) {
                    Halogen_VDom_Util.setAttribute(Data_Nullable.toNullable(v2.value0), v2.value1, v2.value2, el);
                    return v2;
                };
                if (v2 instanceof Property) {
                    setProperty(v2.value0, v2.value1, el);
                    return v2;
                };
                if (v2 instanceof Handler) {
                    var v3 = Halogen_VDom_Util.unsafeGetAny(v2.value0, events);
                    if (Halogen_VDom_Util.unsafeHasAny(v2.value0, events)) {
                        Effect_Ref.write(v2.value1)(Data_Tuple.snd(v3))();
                        return v2;
                    };
                    var ref = Effect_Ref["new"](v2.value1)();
                    var listener = Web_Event_EventTarget.eventListener(function (ev) {
                        return function __do() {
                            var f$prime = Effect_Ref.read(ref)();
                            return mbEmit(f$prime(ev));
                        };
                    })();
                    Halogen_VDom_Util.pokeMutMap(v2.value0, new Data_Tuple.Tuple(listener, ref), events);
                    Halogen_VDom_Util.addEventListener(v2.value0, listener, el);
                    return v2;
                };
                if (v2 instanceof Ref) {
                    mbEmit(v2.value0(new Created(el)));
                    return v2;
                };
                throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 113, column 5 - line 135, column 15): " + [ v2.constructor.name ]);
            };
        };
        var patchProp = function (state, ps2) {
            var events = Halogen_VDom_Util.newMutMap();
            var onThis = removeProp(state.events);
            var onThese = diffProp(state.events, events);
            var onThat = applyProp(events);
            var props = Halogen_VDom_Util.diffWithKeyAndIxE(state.props, ps2, propToStrKey, onThese, onThis, onThat);
            var nextState = {
                events: Halogen_VDom_Util.unsafeFreeze(events),
                props: props
            };
            return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(Data_Unit.unit, nextState, patchProp, haltProp));
        };
        var renderProp = function (ps1) {
            var events = Halogen_VDom_Util.newMutMap();
            var ps1$prime = Halogen_VDom_Util.strMapWithIxE(ps1, propToStrKey, applyProp(events));
            var state = {
                events: Halogen_VDom_Util.unsafeFreeze(events),
                props: ps1$prime
            };
            return Halogen_VDom_Machine.mkStep(new Halogen_VDom_Machine.Step(Data_Unit.unit, state, patchProp, haltProp));
        };
        return renderProp;
    };
};
module.exports = {
    Attribute: Attribute,
    Property: Property,
    Handler: Handler,
    Ref: Ref,
    Created: Created,
    Removed: Removed,
    propFromString: propFromString,
    propFromBoolean: propFromBoolean,
    propFromInt: propFromInt,
    propFromNumber: propFromNumber,
    buildProp: buildProp,
    functorProp: functorProp,
    functorElemRef: functorElemRef
};
