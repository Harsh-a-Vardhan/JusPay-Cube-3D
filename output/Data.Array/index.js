// Generated by purs version 0.14.7
"use strict";
var $foreign = require("./foreign.js");
var Control_Alt = require("../Control.Alt/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Category = require("../Control.Category/index.js");
var Control_Lazy = require("../Control.Lazy/index.js");
var Control_Monad_Rec_Class = require("../Control.Monad.Rec.Class/index.js");
var Control_Monad_ST_Internal = require("../Control.Monad.ST.Internal/index.js");
var Data_Array_ST = require("../Data.Array.ST/index.js");
var Data_Array_ST_Iterator = require("../Data.Array.ST.Iterator/index.js");
var Data_Boolean = require("../Data.Boolean/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ordering = require("../Data.Ordering/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Data_Unfoldable = require("../Data.Unfoldable/index.js");
var zipWithA = function (dictApplicative) {
    return function (f) {
        return function (xs) {
            return function (ys) {
                return Data_Traversable.sequence(Data_Traversable.traversableArray)(dictApplicative)($foreign.zipWith(f)(xs)(ys));
            };
        };
    };
};
var zip = $foreign.zipWith(Data_Tuple.Tuple.create);
var updateAtIndices = function (dictFoldable) {
    return function (us) {
        return function (xs) {
            return Data_Array_ST.withArray(function (res) {
                return Data_Foldable.traverse_(Control_Monad_ST_Internal.applicativeST)(dictFoldable)(function (v) {
                    return Data_Array_ST.poke(v.value0)(v.value1)(res);
                })(us);
            })(xs)();
        };
    };
};
var updateAt = $foreign["_updateAt"](Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var unsafeIndex = function () {
    return $foreign.unsafeIndexImpl;
};
var uncons = $foreign.unconsImpl(Data_Function["const"](Data_Maybe.Nothing.value))(function (x) {
    return function (xs) {
        return new Data_Maybe.Just({
            head: x,
            tail: xs
        });
    };
});
var toUnfoldable = function (dictUnfoldable) {
    return function (xs) {
        var len = $foreign.length(xs);
        var f = function (i) {
            if (i < len) {
                return new Data_Maybe.Just(new Data_Tuple.Tuple(unsafeIndex()(xs)(i), i + 1 | 0));
            };
            if (Data_Boolean.otherwise) {
                return Data_Maybe.Nothing.value;
            };
            throw new Error("Failed pattern match at Data.Array (line 158, column 3 - line 160, column 26): " + [ i.constructor.name ]);
        };
        return Data_Unfoldable.unfoldr(dictUnfoldable)(f)(0);
    };
};
var take = function (n) {
    return function (xs) {
        var $58 = n < 1;
        if ($58) {
            return [  ];
        };
        return $foreign.slice(0)(n)(xs);
    };
};
var tail = $foreign.unconsImpl(Data_Function["const"](Data_Maybe.Nothing.value))(function (v) {
    return function (xs) {
        return new Data_Maybe.Just(xs);
    };
});
var splitAt = function (i) {
    return function (xs) {
        if (i <= 0) {
            return {
                before: [  ],
                after: xs
            };
        };
        return {
            before: $foreign.slice(0)(i)(xs),
            after: $foreign.slice(i)($foreign.length(xs))(xs)
        };
    };
};
var sortBy = function (comp) {
    return $foreign.sortByImpl(comp)(function (v) {
        if (v instanceof Data_Ordering.GT) {
            return 1;
        };
        if (v instanceof Data_Ordering.EQ) {
            return 0;
        };
        if (v instanceof Data_Ordering.LT) {
            return -1 | 0;
        };
        throw new Error("Failed pattern match at Data.Array (line 831, column 31 - line 834, column 11): " + [ v.constructor.name ]);
    });
};
var sortWith = function (dictOrd) {
    return function (f) {
        return sortBy(Data_Ord.comparing(dictOrd)(f));
    };
};
var sort = function (dictOrd) {
    return function (xs) {
        return sortBy(Data_Ord.compare(dictOrd))(xs);
    };
};
var snoc = function (xs) {
    return function (x) {
        return Data_Array_ST.withArray(Data_Array_ST.push(x))(xs)();
    };
};
var singleton = function (a) {
    return [ a ];
};
var $$null = function (xs) {
    return $foreign.length(xs) === 0;
};
var nubByEq = function (eq) {
    return function (xs) {
        return (function __do() {
            var arr = Data_Array_ST["new"]();
            Control_Monad_ST_Internal.foreach(xs)(function (x) {
                return function __do() {
                    var e = Data_Functor.map(Control_Monad_ST_Internal.functorST)((function () {
                        var $90 = Data_HeytingAlgebra.not(Data_HeytingAlgebra.heytingAlgebraBoolean);
                        var $91 = $foreign.any(function (v) {
                            return eq(v)(x);
                        });
                        return function ($92) {
                            return $90($91($92));
                        };
                    })())(Data_Array_ST.unsafeFreeze(arr))();
                    return Control_Applicative.when(Control_Monad_ST_Internal.applicativeST)(e)(Data_Functor["void"](Control_Monad_ST_Internal.functorST)(Data_Array_ST.push(x)(arr)))();
                };
            })();
            return Data_Array_ST.unsafeFreeze(arr)();
        })();
    };
};
var nubEq = function (dictEq) {
    return nubByEq(Data_Eq.eq(dictEq));
};
var modifyAtIndices = function (dictFoldable) {
    return function (is) {
        return function (f) {
            return function (xs) {
                return Data_Array_ST.withArray(function (res) {
                    return Data_Foldable.traverse_(Control_Monad_ST_Internal.applicativeST)(dictFoldable)(function (i) {
                        return Data_Array_ST.modify(i)(f)(res);
                    })(is);
                })(xs)();
            };
        };
    };
};
var mapWithIndex = function (f) {
    return function (xs) {
        return $foreign.zipWith(f)($foreign.range(0)($foreign.length(xs) - 1 | 0))(xs);
    };
};
var intersperse = function (a) {
    return function (arr) {
        var v = $foreign.length(arr);
        if (v < 2) {
            return arr;
        };
        if (Data_Boolean.otherwise) {
            return Data_Array_ST.run((function () {
                var unsafeGetElem = function (idx) {
                    return unsafeIndex()(arr)(idx);
                };
                return function __do() {
                    var out = Data_Array_ST["new"]();
                    Data_Array_ST.push(unsafeGetElem(0))(out)();
                    Control_Monad_ST_Internal["for"](1)(v)(function (idx) {
                        return function __do() {
                            Data_Array_ST.push(a)(out)();
                            return Data_Functor["void"](Control_Monad_ST_Internal.functorST)(Data_Array_ST.push(unsafeGetElem(idx))(out))();
                        };
                    })();
                    return out;
                };
            })());
        };
        throw new Error("Failed pattern match at Data.Array (line 613, column 21 - line 622, column 19): " + [ v.constructor.name ]);
    };
};
var intercalate = function (dictMonoid) {
    return Data_Foldable.intercalate(Data_Foldable.foldableArray)(dictMonoid);
};
var insertAt = $foreign["_insertAt"](Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var init = function (xs) {
    if ($$null(xs)) {
        return Data_Maybe.Nothing.value;
    };
    if (Data_Boolean.otherwise) {
        return new Data_Maybe.Just($foreign.slice(0)($foreign.length(xs) - 1 | 0)(xs));
    };
    throw new Error("Failed pattern match at Data.Array (line 340, column 1 - line 340, column 45): " + [ xs.constructor.name ]);
};
var index = $foreign.indexImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var last = function (xs) {
    return index(xs)($foreign.length(xs) - 1 | 0);
};
var unsnoc = function (xs) {
    return Control_Apply.apply(Data_Maybe.applyMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)(function (v) {
        return function (v1) {
            return {
                init: v,
                last: v1
            };
        };
    })(init(xs)))(last(xs));
};
var modifyAt = function (i) {
    return function (f) {
        return function (xs) {
            var go = function (x) {
                return updateAt(i)(f(x))(xs);
            };
            return Data_Maybe.maybe(Data_Maybe.Nothing.value)(go)(index(xs)(i));
        };
    };
};
var span = function (p) {
    return function (arr) {
        var go = function ($copy_i) {
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(i) {
                var v = index(arr)(i);
                if (v instanceof Data_Maybe.Just) {
                    var $65 = p(v.value0);
                    if ($65) {
                        $copy_i = i + 1 | 0;
                        return;
                    };
                    $tco_done = true;
                    return new Data_Maybe.Just(i);
                };
                if (v instanceof Data_Maybe.Nothing) {
                    $tco_done = true;
                    return Data_Maybe.Nothing.value;
                };
                throw new Error("Failed pattern match at Data.Array (line 966, column 5 - line 968, column 25): " + [ v.constructor.name ]);
            };
            while (!$tco_done) {
                $tco_result = $tco_loop($copy_i);
            };
            return $tco_result;
        };
        var breakIndex = go(0);
        if (breakIndex instanceof Data_Maybe.Just && breakIndex.value0 === 0) {
            return {
                init: [  ],
                rest: arr
            };
        };
        if (breakIndex instanceof Data_Maybe.Just) {
            return {
                init: $foreign.slice(0)(breakIndex.value0)(arr),
                rest: $foreign.slice(breakIndex.value0)($foreign.length(arr))(arr)
            };
        };
        if (breakIndex instanceof Data_Maybe.Nothing) {
            return {
                init: arr,
                rest: [  ]
            };
        };
        throw new Error("Failed pattern match at Data.Array (line 953, column 3 - line 959, column 30): " + [ breakIndex.constructor.name ]);
    };
};
var takeWhile = function (p) {
    return function (xs) {
        return (span(p)(xs)).init;
    };
};
var unzip = function (xs) {
    return (function __do() {
        var fsts = Data_Array_ST["new"]();
        var snds = Data_Array_ST["new"]();
        var iter = Data_Array_ST_Iterator.iterator(function (v) {
            return index(xs)(v);
        })();
        Data_Array_ST_Iterator.iterate(iter)(function (v) {
            return function __do() {
                Data_Functor["void"](Control_Monad_ST_Internal.functorST)(Data_Array_ST.push(v.value0)(fsts))();
                return Data_Functor["void"](Control_Monad_ST_Internal.functorST)(Data_Array_ST.push(v.value1)(snds))();
            };
        })();
        var fsts$prime = Data_Array_ST.unsafeFreeze(fsts)();
        var snds$prime = Data_Array_ST.unsafeFreeze(snds)();
        return new Data_Tuple.Tuple(fsts$prime, snds$prime);
    })();
};
var head = function (xs) {
    return index(xs)(0);
};
var nubBy = function (comp) {
    return function (xs) {
        var indexedAndSorted = sortBy(function (x) {
            return function (y) {
                return comp(Data_Tuple.snd(x))(Data_Tuple.snd(y));
            };
        })(mapWithIndex(Data_Tuple.Tuple.create)(xs));
        var v = head(indexedAndSorted);
        if (v instanceof Data_Maybe.Nothing) {
            return [  ];
        };
        if (v instanceof Data_Maybe.Just) {
            return Data_Functor.map(Data_Functor.functorArray)(Data_Tuple.snd)(sortWith(Data_Ord.ordInt)(Data_Tuple.fst)((function __do() {
                var result = Data_Array_ST.unsafeThaw(singleton(v.value0))();
                Control_Monad_ST_Internal.foreach(indexedAndSorted)(function (v1) {
                    return function __do() {
                        var lst = Data_Functor.map(Control_Monad_ST_Internal.functorST)((function () {
                            var $93 = (function () {
                                var $95 = Data_Maybe.fromJust();
                                return function ($96) {
                                    return $95(last($96));
                                };
                            })();
                            return function ($94) {
                                return Data_Tuple.snd($93($94));
                            };
                        })())(Data_Array_ST.unsafeFreeze(result))();
                        return Control_Applicative.when(Control_Monad_ST_Internal.applicativeST)(Data_Eq.notEq(Data_Ordering.eqOrdering)(comp(lst)(v1.value1))(Data_Ordering.EQ.value))(Data_Functor["void"](Control_Monad_ST_Internal.functorST)(Data_Array_ST.push(v1)(result)))();
                    };
                })();
                return Data_Array_ST.unsafeFreeze(result)();
            })()));
        };
        throw new Error("Failed pattern match at Data.Array (line 1050, column 17 - line 1058, column 29): " + [ v.constructor.name ]);
    };
};
var nub = function (dictOrd) {
    return nubBy(Data_Ord.compare(dictOrd));
};
var groupBy = function (op) {
    return function (xs) {
        return (function __do() {
            var result = Data_Array_ST["new"]();
            var iter = Data_Array_ST_Iterator.iterator(function (v) {
                return index(xs)(v);
            })();
            Data_Array_ST_Iterator.iterate(iter)(function (x) {
                return Data_Functor["void"](Control_Monad_ST_Internal.functorST)(function __do() {
                    var sub = Data_Array_ST["new"]();
                    Data_Array_ST.push(x)(sub)();
                    Data_Array_ST_Iterator.pushWhile(op(x))(iter)(sub)();
                    var grp = Data_Array_ST.unsafeFreeze(sub)();
                    return Data_Array_ST.push(grp)(result)();
                });
            })();
            return Data_Array_ST.unsafeFreeze(result)();
        })();
    };
};
var groupAllBy = function (cmp) {
    var $97 = groupBy(function (x) {
        return function (y) {
            return Data_Eq.eq(Data_Ordering.eqOrdering)(cmp(x)(y))(Data_Ordering.EQ.value);
        };
    });
    var $98 = sortBy(cmp);
    return function ($99) {
        return $97($98($99));
    };
};
var groupAll = function (dictOrd) {
    return groupAllBy(Data_Ord.compare(dictOrd));
};
var group$prime = function () {
    return function (dictOrd) {
        return groupAll(dictOrd);
    };
};
var group = function (dictEq) {
    return function (xs) {
        return groupBy(Data_Eq.eq(dictEq))(xs);
    };
};
var fromFoldable = function (dictFoldable) {
    return $foreign.fromFoldableImpl(Data_Foldable.foldr(dictFoldable));
};
var foldr = Data_Foldable.foldr(Data_Foldable.foldableArray);
var foldl = Data_Foldable.foldl(Data_Foldable.foldableArray);
var foldRecM = function (dictMonadRec) {
    return function (f) {
        return function (b) {
            return function (array) {
                var go = function (res) {
                    return function (i) {
                        if (i >= $foreign.length(array)) {
                            return Control_Applicative.pure((dictMonadRec.Monad0()).Applicative0())(new Control_Monad_Rec_Class.Done(res));
                        };
                        if (Data_Boolean.otherwise) {
                            return Control_Bind.bind((dictMonadRec.Monad0()).Bind1())(f(res)(unsafeIndex()(array)(i)))(function (res$prime) {
                                return Control_Applicative.pure((dictMonadRec.Monad0()).Applicative0())(new Control_Monad_Rec_Class.Loop({
                                    a: res$prime,
                                    b: i + 1 | 0
                                }));
                            });
                        };
                        throw new Error("Failed pattern match at Data.Array (line 1269, column 3 - line 1273, column 42): " + [ res.constructor.name, i.constructor.name ]);
                    };
                };
                return Control_Monad_Rec_Class.tailRecM2(dictMonadRec)(go)(b)(0);
            };
        };
    };
};
var foldMap = function (dictMonoid) {
    return Data_Foldable.foldMap(Data_Foldable.foldableArray)(dictMonoid);
};
var foldM = function (dictMonad) {
    return function (f) {
        return function (b) {
            return $foreign.unconsImpl(function (v) {
                return Control_Applicative.pure(dictMonad.Applicative0())(b);
            })(function (a) {
                return function (as) {
                    return Control_Bind.bind(dictMonad.Bind1())(f(b)(a))(function (b$prime) {
                        return foldM(dictMonad)(f)(b$prime)(as);
                    });
                };
            });
        };
    };
};
var fold = function (dictMonoid) {
    return Data_Foldable.fold(Data_Foldable.foldableArray)(dictMonoid);
};
var findMap = $foreign.findMapImpl(Data_Maybe.Nothing.value)(Data_Maybe.isJust);
var findLastIndex = $foreign.findLastIndexImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var insertBy = function (cmp) {
    return function (x) {
        return function (ys) {
            var i = Data_Maybe.maybe(0)(function (v) {
                return v + 1 | 0;
            })(findLastIndex(function (y) {
                return Data_Eq.eq(Data_Ordering.eqOrdering)(cmp(x)(y))(Data_Ordering.GT.value);
            })(ys));
            return Data_Maybe.fromJust()(insertAt(i)(x)(ys));
        };
    };
};
var insert = function (dictOrd) {
    return insertBy(Data_Ord.compare(dictOrd));
};
var findIndex = $foreign.findIndexImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var intersectBy = function (eq) {
    return function (xs) {
        return function (ys) {
            return $foreign.filter(function (x) {
                return Data_Maybe.isJust(findIndex(eq(x))(ys));
            })(xs);
        };
    };
};
var intersect = function (dictEq) {
    return intersectBy(Data_Eq.eq(dictEq));
};
var find = function (f) {
    return function (xs) {
        return Data_Functor.map(Data_Maybe.functorMaybe)(unsafeIndex()(xs))(findIndex(f)(xs));
    };
};
var elemLastIndex = function (dictEq) {
    return function (x) {
        return findLastIndex(function (v) {
            return Data_Eq.eq(dictEq)(v)(x);
        });
    };
};
var elemIndex = function (dictEq) {
    return function (x) {
        return findIndex(function (v) {
            return Data_Eq.eq(dictEq)(v)(x);
        });
    };
};
var notElem = function (dictEq) {
    return function (a) {
        return function (arr) {
            return Data_Maybe.isNothing(elemIndex(dictEq)(a)(arr));
        };
    };
};
var elem = function (dictEq) {
    return function (a) {
        return function (arr) {
            return Data_Maybe.isJust(elemIndex(dictEq)(a)(arr));
        };
    };
};
var dropWhile = function (p) {
    return function (xs) {
        return (span(p)(xs)).rest;
    };
};
var dropEnd = function (n) {
    return function (xs) {
        return take($foreign.length(xs) - n | 0)(xs);
    };
};
var drop = function (n) {
    return function (xs) {
        var $80 = n < 1;
        if ($80) {
            return xs;
        };
        return $foreign.slice(n)($foreign.length(xs))(xs);
    };
};
var takeEnd = function (n) {
    return function (xs) {
        return drop($foreign.length(xs) - n | 0)(xs);
    };
};
var deleteAt = $foreign["_deleteAt"](Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var deleteBy = function (v) {
    return function (v1) {
        return function (v2) {
            if (v2.length === 0) {
                return [  ];
            };
            return Data_Maybe.maybe(v2)(function (i) {
                return Data_Maybe.fromJust()(deleteAt(i)(v2));
            })(findIndex(v(v1))(v2));
        };
    };
};
var unionBy = function (eq) {
    return function (xs) {
        return function (ys) {
            return Data_Semigroup.append(Data_Semigroup.semigroupArray)(xs)(foldl(Data_Function.flip(deleteBy(eq)))(nubByEq(eq)(ys))(xs));
        };
    };
};
var union = function (dictEq) {
    return unionBy(Data_Eq.eq(dictEq));
};
var $$delete = function (dictEq) {
    return deleteBy(Data_Eq.eq(dictEq));
};
var difference = function (dictEq) {
    return foldr($$delete(dictEq));
};
var cons = function (x) {
    return function (xs) {
        return Data_Semigroup.append(Data_Semigroup.semigroupArray)([ x ])(xs);
    };
};
var some = function (dictAlternative) {
    return function (dictLazy) {
        return function (v) {
            return Control_Apply.apply((dictAlternative.Applicative0()).Apply0())(Data_Functor.map(((dictAlternative.Plus1()).Alt0()).Functor0())(cons)(v))(Control_Lazy.defer(dictLazy)(function (v1) {
                return many(dictAlternative)(dictLazy)(v);
            }));
        };
    };
};
var many = function (dictAlternative) {
    return function (dictLazy) {
        return function (v) {
            return Control_Alt.alt((dictAlternative.Plus1()).Alt0())(some(dictAlternative)(dictLazy)(v))(Control_Applicative.pure(dictAlternative.Applicative0())([  ]));
        };
    };
};
var concatMap = Data_Function.flip(Control_Bind.bind(Control_Bind.bindArray));
var mapMaybe = function (f) {
    return concatMap((function () {
        var $100 = Data_Maybe.maybe([  ])(singleton);
        return function ($101) {
            return $100(f($101));
        };
    })());
};
var filterA = function (dictApplicative) {
    return function (p) {
        var $102 = Data_Functor.map((dictApplicative.Apply0()).Functor0())(mapMaybe(function (v) {
            if (v.value1) {
                return new Data_Maybe.Just(v.value0);
            };
            return Data_Maybe.Nothing.value;
        }));
        var $103 = Data_Traversable.traverse(Data_Traversable.traversableArray)(dictApplicative)(function (x) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Tuple.Tuple.create(x))(p(x));
        });
        return function ($104) {
            return $102($103($104));
        };
    };
};
var catMaybes = mapMaybe(Control_Category.identity(Control_Category.categoryFn));
var alterAt = function (i) {
    return function (f) {
        return function (xs) {
            var go = function (x) {
                var v = f(x);
                if (v instanceof Data_Maybe.Nothing) {
                    return deleteAt(i)(xs);
                };
                if (v instanceof Data_Maybe.Just) {
                    return updateAt(i)(v.value0)(xs);
                };
                throw new Error("Failed pattern match at Data.Array (line 591, column 10 - line 593, column 32): " + [ v.constructor.name ]);
            };
            return Data_Maybe.maybe(Data_Maybe.Nothing.value)(go)(index(xs)(i));
        };
    };
};
module.exports = {
    fromFoldable: fromFoldable,
    toUnfoldable: toUnfoldable,
    singleton: singleton,
    some: some,
    many: many,
    "null": $$null,
    cons: cons,
    snoc: snoc,
    insert: insert,
    insertBy: insertBy,
    head: head,
    last: last,
    tail: tail,
    init: init,
    uncons: uncons,
    unsnoc: unsnoc,
    index: index,
    elem: elem,
    notElem: notElem,
    elemIndex: elemIndex,
    elemLastIndex: elemLastIndex,
    find: find,
    findMap: findMap,
    findIndex: findIndex,
    findLastIndex: findLastIndex,
    insertAt: insertAt,
    deleteAt: deleteAt,
    updateAt: updateAt,
    updateAtIndices: updateAtIndices,
    modifyAt: modifyAt,
    modifyAtIndices: modifyAtIndices,
    alterAt: alterAt,
    intersperse: intersperse,
    concatMap: concatMap,
    splitAt: splitAt,
    filterA: filterA,
    mapMaybe: mapMaybe,
    catMaybes: catMaybes,
    mapWithIndex: mapWithIndex,
    foldl: foldl,
    foldr: foldr,
    foldMap: foldMap,
    fold: fold,
    intercalate: intercalate,
    sort: sort,
    sortBy: sortBy,
    sortWith: sortWith,
    take: take,
    takeEnd: takeEnd,
    takeWhile: takeWhile,
    drop: drop,
    dropEnd: dropEnd,
    dropWhile: dropWhile,
    span: span,
    group: group,
    groupAll: groupAll,
    "group'": group$prime,
    groupBy: groupBy,
    groupAllBy: groupAllBy,
    nub: nub,
    nubEq: nubEq,
    nubBy: nubBy,
    nubByEq: nubByEq,
    union: union,
    unionBy: unionBy,
    "delete": $$delete,
    deleteBy: deleteBy,
    difference: difference,
    intersect: intersect,
    intersectBy: intersectBy,
    zipWithA: zipWithA,
    zip: zip,
    unzip: unzip,
    foldM: foldM,
    foldRecM: foldRecM,
    unsafeIndex: unsafeIndex,
    range: $foreign.range,
    replicate: $foreign.replicate,
    length: $foreign.length,
    reverse: $foreign.reverse,
    concat: $foreign.concat,
    filter: $foreign.filter,
    partition: $foreign.partition,
    scanl: $foreign.scanl,
    scanr: $foreign.scanr,
    slice: $foreign.slice,
    zipWith: $foreign.zipWith,
    any: $foreign.any,
    all: $foreign.all
};
