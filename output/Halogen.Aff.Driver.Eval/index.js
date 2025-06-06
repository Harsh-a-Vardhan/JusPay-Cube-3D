// Generated by purs version 0.14.7
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Applicative_Free = require("../Control.Applicative.Free/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Monad = require("../Control.Monad/index.js");
var Control_Monad_Fork_Class = require("../Control.Monad.Fork.Class/index.js");
var Control_Monad_Free = require("../Control.Monad.Free/index.js");
var Control_Parallel = require("../Control.Parallel/index.js");
var Control_Parallel_Class = require("../Control.Parallel.Class/index.js");
var Data_Boolean = require("../Data.Boolean/index.js");
var Data_Coyoneda = require("../Data.Coyoneda/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_List_Types = require("../Data.List.Types/index.js");
var Data_Map_Internal = require("../Data.Map.Internal/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Effect = require("../Effect/index.js");
var Effect_Aff = require("../Effect.Aff/index.js");
var Effect_Class = require("../Effect.Class/index.js");
var Effect_Exception = require("../Effect.Exception/index.js");
var Effect_Ref = require("../Effect.Ref/index.js");
var Halogen_Aff_Driver_State = require("../Halogen.Aff.Driver.State/index.js");
var Halogen_Query_ChildQuery = require("../Halogen.Query.ChildQuery/index.js");
var Halogen_Query_HalogenM = require("../Halogen.Query.HalogenM/index.js");
var Halogen_Query_HalogenQ = require("../Halogen.Query.HalogenQ/index.js");
var Halogen_Query_Input = require("../Halogen.Query.Input/index.js");
var Halogen_Subscription = require("../Halogen.Subscription/index.js");
var Unsafe_Reference = require("../Unsafe.Reference/index.js");
var unsubscribe = function (sid) {
    return function (ref) {
        return function __do() {
            var v = Effect_Ref.read(ref)();
            var subs = Effect_Ref.read(v.subscriptions)();
            return Data_Foldable.traverse_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(Halogen_Subscription.unsubscribe)(Control_Bind.bindFlipped(Data_Maybe.bindMaybe)(Data_Map_Internal.lookup(Halogen_Query_HalogenM.ordSubscriptionId)(sid))(subs))();
        };
    };
};
var queueOrRun = function (ref) {
    return function (au) {
        return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(ref)))(function (v) {
            if (v instanceof Data_Maybe.Nothing) {
                return au;
            };
            if (v instanceof Data_Maybe.Just) {
                return Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.write(new Data_Maybe.Just(new Data_List_Types.Cons(au, v.value0)))(ref));
            };
            throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 182, column 33 - line 184, column 57): " + [ v.constructor.name ]);
        });
    };
};
var handleLifecycle = function (lchs) {
    return function (f) {
        return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.write({
            initializers: Data_List_Types.Nil.value,
            finalizers: Data_List_Types.Nil.value
        })(lchs)))(function () {
            return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(f))(function (result) {
                return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(lchs)))(function (v) {
                    return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Data_Foldable.traverse_(Effect_Aff.applicativeAff)(Data_List_Types.foldableList)(Control_Monad_Fork_Class.fork(Control_Monad_Fork_Class.monadForkAff))(v.finalizers))(function () {
                        return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Control_Parallel.parSequence_(Effect_Aff.parallelAff)(Data_List_Types.foldableList)(v.initializers))(function () {
                            return Control_Applicative.pure(Effect_Aff.applicativeAff)(result);
                        });
                    });
                });
            });
        });
    };
};
var handleAff = Effect_Aff.runAff_(Data_Either.either(Effect_Exception.throwException)(Data_Function["const"](Control_Applicative.pure(Effect.applicativeEffect)(Data_Unit.unit))));
var fresh = function (f) {
    return function (ref) {
        return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(ref)))(function (v) {
            return Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref["modify'"](function (i) {
                return {
                    state: i + 1 | 0,
                    value: f(i)
                };
            })(v.fresh));
        });
    };
};
var evalQ = function (render) {
    return function (ref) {
        return function (q) {
            return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(ref)))(function (v) {
                return evalM(render)(ref)(v["component"]["eval"](new Halogen_Query_HalogenQ.Query(Data_Functor.map(Data_Coyoneda.functorCoyoneda)(Data_Maybe.Just.create)(Data_Coyoneda.liftCoyoneda(q)), Data_Function["const"](Data_Maybe.Nothing.value))));
            });
        };
    };
};
var evalM = function (render) {
    return function (initRef) {
        return function (v) {
            var evalChildQuery = function (ref) {
                return function (cqb) {
                    return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(ref)))(function (v1) {
                        return Halogen_Query_ChildQuery.unChildQueryBox(function (v2) {
                            var evalChild = function (v3) {
                                return Control_Parallel_Class.parallel(Effect_Aff.parallelAff)(Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(v3)))(function (dsx) {
                                    return Halogen_Aff_Driver_State.unDriverStateX(function (ds) {
                                        return evalQ(render)(ds.selfRef)(v2.value1);
                                    })(dsx);
                                }));
                            };
                            return Data_Functor.map(Effect_Aff.functorAff)(v2.value2)(Control_Parallel_Class.sequential(Effect_Aff.parallelAff)(v2.value0(Effect_Aff.applicativeParAff)(evalChild)(v1.children)));
                        })(cqb);
                    });
                };
            };
            var go = function (ref) {
                return function (v1) {
                    if (v1 instanceof Halogen_Query_HalogenM.State) {
                        return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(ref)))(function (v2) {
                            var v3 = v1.value0(v2.state);
                            if (Unsafe_Reference.unsafeRefEq(v2.state)(v3.value1)) {
                                return Control_Applicative.pure(Effect_Aff.applicativeAff)(v3.value0);
                            };
                            if (Data_Boolean.otherwise) {
                                return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.write({
                                    component: v2.component,
                                    state: v3.value1,
                                    refs: v2.refs,
                                    children: v2.children,
                                    childrenIn: v2.childrenIn,
                                    childrenOut: v2.childrenOut,
                                    selfRef: v2.selfRef,
                                    handlerRef: v2.handlerRef,
                                    pendingQueries: v2.pendingQueries,
                                    pendingOuts: v2.pendingOuts,
                                    pendingHandlers: v2.pendingHandlers,
                                    rendering: v2.rendering,
                                    fresh: v2.fresh,
                                    subscriptions: v2.subscriptions,
                                    forks: v2.forks,
                                    lifecycleHandlers: v2.lifecycleHandlers
                                })(ref)))(function () {
                                    return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(handleLifecycle(v2.lifecycleHandlers)(render(v2.lifecycleHandlers)(ref)))(function () {
                                        return Control_Applicative.pure(Effect_Aff.applicativeAff)(v3.value0);
                                    });
                                });
                            };
                            throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 86, column 7 - line 92, column 21): " + [ v3.constructor.name ]);
                        });
                    };
                    if (v1 instanceof Halogen_Query_HalogenM.Subscribe) {
                        return Control_Bind.bind(Effect_Aff.bindAff)(fresh(Halogen_Query_HalogenM.SubscriptionId)(ref))(function (sid) {
                            return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Halogen_Subscription.subscribe(v1.value0(sid))(function (act) {
                                return handleAff(evalF(render)(ref)(new Halogen_Query_Input.Action(act)));
                            })))(function (finalize) {
                                return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(ref)))(function (v2) {
                                    return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.modify_(Data_Functor.map(Data_Maybe.functorMaybe)(Data_Map_Internal.insert(Halogen_Query_HalogenM.ordSubscriptionId)(sid)(finalize)))(v2.subscriptions)))(function () {
                                        return Control_Applicative.pure(Effect_Aff.applicativeAff)(v1.value1(sid));
                                    });
                                });
                            });
                        });
                    };
                    if (v1 instanceof Halogen_Query_HalogenM.Unsubscribe) {
                        return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(unsubscribe(v1.value0)(ref)))(function () {
                            return Control_Applicative.pure(Effect_Aff.applicativeAff)(v1.value1);
                        });
                    };
                    if (v1 instanceof Halogen_Query_HalogenM.Lift) {
                        return v1.value0;
                    };
                    if (v1 instanceof Halogen_Query_HalogenM.ChildQuery) {
                        return evalChildQuery(ref)(v1.value0);
                    };
                    if (v1 instanceof Halogen_Query_HalogenM.Raise) {
                        return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(ref)))(function (v2) {
                            return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(v2.handlerRef)))(function (handler) {
                                return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(queueOrRun(v2.pendingOuts)(handler(v1.value0)))(function () {
                                    return Control_Applicative.pure(Effect_Aff.applicativeAff)(v1.value1);
                                });
                            });
                        });
                    };
                    if (v1 instanceof Halogen_Query_HalogenM.Par) {
                        return Control_Parallel_Class.sequential(Effect_Aff.parallelAff)(Control_Applicative_Free.retractFreeAp(Effect_Aff.applicativeParAff)(Control_Applicative_Free.hoistFreeAp((function () {
                            var $79 = Control_Parallel_Class.parallel(Effect_Aff.parallelAff);
                            var $80 = evalM(render)(ref);
                            return function ($81) {
                                return $79($80($81));
                            };
                        })())(v1.value0)));
                    };
                    if (v1 instanceof Halogen_Query_HalogenM.Fork) {
                        return Control_Bind.bind(Effect_Aff.bindAff)(fresh(Halogen_Query_HalogenM.ForkId)(ref))(function (fid) {
                            return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(ref)))(function (v2) {
                                return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref["new"](false)))(function (doneRef) {
                                    return Control_Bind.bind(Effect_Aff.bindAff)(Control_Monad_Fork_Class.fork(Control_Monad_Fork_Class.monadForkAff)(Effect_Aff["finally"](Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(function __do() {
                                        Effect_Ref.modify_(Data_Map_Internal["delete"](Halogen_Query_HalogenM.ordForkId)(fid))(v2.forks)();
                                        return Effect_Ref.write(true)(doneRef)();
                                    }))(evalM(render)(ref)(v1.value0))))(function (fiber) {
                                        return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Control_Monad.unlessM(Effect.monadEffect)(Effect_Ref.read(doneRef))(Effect_Ref.modify_(Data_Map_Internal.insert(Halogen_Query_HalogenM.ordForkId)(fid)(fiber))(v2.forks))))(function () {
                                            return Control_Applicative.pure(Effect_Aff.applicativeAff)(v1.value1(fid));
                                        });
                                    });
                                });
                            });
                        });
                    };
                    if (v1 instanceof Halogen_Query_HalogenM.Kill) {
                        return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(ref)))(function (v2) {
                            return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(v2.forks)))(function (forkMap) {
                                return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Data_Foldable.traverse_(Effect_Aff.applicativeAff)(Data_Foldable.foldableMaybe)(Effect_Aff.killFiber(Effect_Exception.error("Cancelled")))(Data_Map_Internal.lookup(Halogen_Query_HalogenM.ordForkId)(v1.value0)(forkMap)))(function () {
                                    return Control_Applicative.pure(Effect_Aff.applicativeAff)(v1.value1);
                                });
                            });
                        });
                    };
                    if (v1 instanceof Halogen_Query_HalogenM.GetRef) {
                        return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(ref)))(function (v2) {
                            return Control_Applicative.pure(Effect_Aff.applicativeAff)(v1.value1(Data_Map_Internal.lookup(Data_Ord.ordString)(v1.value0)(v2.refs)));
                        });
                    };
                    throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 83, column 12 - line 133, column 33): " + [ v1.constructor.name ]);
                };
            };
            return Control_Monad_Free.foldFree(Effect_Aff.monadRecAff)(go(initRef))(v);
        };
    };
};
var evalF = function (render) {
    return function (ref) {
        return function (v) {
            if (v instanceof Halogen_Query_Input.RefUpdate) {
                return Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Data_Function.flip(Effect_Ref.modify_)(ref)(Halogen_Aff_Driver_State.mapDriverState(function (st) {
                    return {
                        component: st.component,
                        state: st.state,
                        refs: Data_Map_Internal.alter(Data_Ord.ordString)(Data_Function["const"](v.value1))(v.value0)(st.refs),
                        children: st.children,
                        childrenIn: st.childrenIn,
                        childrenOut: st.childrenOut,
                        selfRef: st.selfRef,
                        handlerRef: st.handlerRef,
                        pendingQueries: st.pendingQueries,
                        pendingOuts: st.pendingOuts,
                        pendingHandlers: st.pendingHandlers,
                        rendering: st.rendering,
                        fresh: st.fresh,
                        subscriptions: st.subscriptions,
                        forks: st.forks,
                        lifecycleHandlers: st.lifecycleHandlers
                    };
                })));
            };
            if (v instanceof Halogen_Query_Input.Action) {
                return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Ref.read(ref)))(function (v1) {
                    return evalM(render)(ref)(v1["component"]["eval"](new Halogen_Query_HalogenQ.Action(v.value0, Data_Unit.unit)));
                });
            };
            throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 52, column 20 - line 58, column 62): " + [ v.constructor.name ]);
        };
    };
};
module.exports = {
    evalF: evalF,
    evalQ: evalQ,
    evalM: evalM,
    handleLifecycle: handleLifecycle,
    queueOrRun: queueOrRun,
    handleAff: handleAff
};
