// Generated by purs version 0.14.7
"use strict";
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_List_Types = require("../Data.List.Types/index.js");
var Data_Map_Internal = require("../Data.Map.Internal/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Effect_Ref = require("../Effect.Ref/index.js");
var Halogen_Data_Slot = require("../Halogen.Data.Slot/index.js");
var Unsafe_Coerce = require("../Unsafe.Coerce/index.js");
var DriverStateRef = function (x) {
    return x;
};
var DriverState = function (x) {
    return x;
};
var unRenderStateX = Unsafe_Coerce.unsafeCoerce;
var unDriverStateX = Unsafe_Coerce.unsafeCoerce;
var renderStateX_ = function (dictApplicative) {
    return function (f) {
        return unDriverStateX(function (st) {
            return Data_Foldable.traverse_(dictApplicative)(Data_Foldable.foldableMaybe)(f)(st.rendering);
        });
    };
};
var mkRenderStateX = Unsafe_Coerce.unsafeCoerce;
var renderStateX = function (dictFunctor) {
    return function (f) {
        return unDriverStateX(function (st) {
            return mkRenderStateX(f(st.rendering));
        });
    };
};
var mkDriverStateXRef = Unsafe_Coerce.unsafeCoerce;
var mapDriverState = function (f) {
    return function (v) {
        return f(v);
    };
};
var initDriverState = function (component) {
    return function (input) {
        return function (handler) {
            return function (lchs) {
                return function __do() {
                    var selfRef = Effect_Ref["new"]({})();
                    var childrenIn = Effect_Ref["new"](Halogen_Data_Slot.empty)();
                    var childrenOut = Effect_Ref["new"](Halogen_Data_Slot.empty)();
                    var handlerRef = Effect_Ref["new"](handler)();
                    var pendingQueries = Effect_Ref["new"](new Data_Maybe.Just(Data_List_Types.Nil.value))();
                    var pendingOuts = Effect_Ref["new"](new Data_Maybe.Just(Data_List_Types.Nil.value))();
                    var pendingHandlers = Effect_Ref["new"](Data_Maybe.Nothing.value)();
                    var fresh = Effect_Ref["new"](1)();
                    var subscriptions = Effect_Ref["new"](new Data_Maybe.Just(Data_Map_Internal.empty))();
                    var forks = Effect_Ref["new"](Data_Map_Internal.empty)();
                    var ds = {
                        component: component,
                        state: component.initialState(input),
                        refs: Data_Map_Internal.empty,
                        children: Halogen_Data_Slot.empty,
                        childrenIn: childrenIn,
                        childrenOut: childrenOut,
                        selfRef: selfRef,
                        handlerRef: handlerRef,
                        pendingQueries: pendingQueries,
                        pendingOuts: pendingOuts,
                        pendingHandlers: pendingHandlers,
                        rendering: Data_Maybe.Nothing.value,
                        fresh: fresh,
                        subscriptions: subscriptions,
                        forks: forks,
                        lifecycleHandlers: lchs
                    };
                    Effect_Ref.write(ds)(selfRef)();
                    return mkDriverStateXRef(selfRef);
                };
            };
        };
    };
};
module.exports = {
    DriverState: DriverState,
    mapDriverState: mapDriverState,
    DriverStateRef: DriverStateRef,
    unDriverStateX: unDriverStateX,
    mkDriverStateXRef: mkDriverStateXRef,
    renderStateX: renderStateX,
    renderStateX_: renderStateX_,
    unRenderStateX: unRenderStateX,
    initDriverState: initDriverState
};
