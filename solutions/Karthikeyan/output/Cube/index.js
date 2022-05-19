"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Monad_State_Class = require("../Control.Monad.State.Class/index.js");
var Data_Array = require("../Data.Array/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Halogen_Component = require("../Halogen.Component/index.js");
var Halogen_HTML_Core = require("../Halogen.HTML.Core/index.js");
var Halogen_HTML_Elements = require("../Halogen.HTML.Elements/index.js");
var Halogen_HTML_Events = require("../Halogen.HTML.Events/index.js");
var Halogen_HTML_Properties = require("../Halogen.HTML.Properties/index.js");
var Halogen_Query_HalogenM = require("../Halogen.Query.HalogenM/index.js");
var Halogen_Svg_Attributes = require("../Halogen.Svg.Attributes/index.js");
var Halogen_Svg_Elements = require("../Halogen.Svg.Elements/index.js");
var $$Math = require("../Math/index.js");
var Tick = (function () {
    function Tick(value0) {
        this.value0 = value0;
    };
    Tick.create = function (value0) {
        return new Tick(value0);
    };
    return Tick;
})();
var Other = (function () {
    function Other(value0) {
        this.value0 = value0;
    };
    Other.create = function (value0) {
        return new Other(value0);
    };
    return Other;
})();
var X = (function () {
    function X() {

    };
    X.value = new X();
    return X;
})();
var Y = (function () {
    function Y() {

    };
    Y.value = new Y();
    return Y;
})();
var Z = (function () {
    function Z() {

    };
    Z.value = new Z();
    return Z;
})();

// Events
var DecAngVelocity = (function () {
    function DecAngVelocity(value0) {
        this.value0 = value0;
    };
    DecAngVelocity.create = function (value0) {
        return new DecAngVelocity(value0);
    };
    return DecAngVelocity;
})();

// Events
var IncAngVelocity = (function () {
    function IncAngVelocity(value0) {
        this.value0 = value0;
    };
    IncAngVelocity.create = function (value0) {
        return new IncAngVelocity(value0);
    };
    return IncAngVelocity;
})();

// Events
var Reversedirection = (function () {
    function Reversedirection(value0) {
        this.value0 = value0;
    };
    Reversedirection.create = function (value0) {
        return new Reversedirection(value0);
    };
    return Reversedirection;
})();

// Events
var IncVelocity = (function () {
    function IncVelocity(value0) {
        this.value0 = value0;
    };
    IncVelocity.create = function (value0) {
        return new IncVelocity(value0);
    };
    return IncVelocity;
})();

// Events
var DecVelocity = (function () {
    function DecVelocity(value0) {
        this.value0 = value0;
    };
    DecVelocity.create = function (value0) {
        return new DecVelocity(value0);
    };
    return DecVelocity;
})();

// Events
var Add = (function () {
    function Add() {

    };
    Add.value = new Add();
    return Add;
})();

// Events
var Del = (function () {
    function Del() {

    };
    Del.value = new Del();
    return Del;
})();

// Values
var viewBoxSize = 600.0;
var viewCenter = {
    x: viewBoxSize / 2.0,
    y: viewBoxSize / 2.0
};
var rotate = function (v) {
    var rotateInPlane = function (axis1) {
        return function (axis2) {
            return function (ang) {
                return new Data_Tuple.Tuple(axis1 * $$Math.cos(ang) - axis2 * $$Math.sin(ang), axis2 * $$Math.cos(ang) + axis1 * $$Math.sin(ang));
            };
        };
    };
    var rotateX = function (ang) {
        return function (v1) {
            var v2 = rotateInPlane(v1.y)(v1.z)(ang);
            return {
                x: v1.x,
                y: v2.value0,
                z: v2.value1
            };
        };
    };
    var rotateY = function (ang) {
        return function (v1) {
            var v2 = rotateInPlane(v1.x)(v1.z)(ang);
            return {
                x: v2.value0,
                y: v1.y,
                z: v2.value1
            };
        };
    };
    var rotateZ = function (ang) {
        return function (v1) {
            var v2 = rotateInPlane(v1.x)(v1.y)(ang);
            return {
                x: v2.value0,
                y: v2.value1,
                z: v1.z
            };
        };
    };
    var $111 = rotateZ(v.za);
    var $112 = rotateY(v.ya);
    var $113 = rotateX(v.xa);
    return function ($114) {
        return $111($112($113($114)));
    };
};
var rotateShape = function (vertices) {
    return function (ang) {
        return Data_Functor.map(Data_Functor.functorArray)(rotate(ang))(vertices);
    };
};
var revDir = function (c) {
    return {
        shape: c.shape,
        angVel: c.angVel,
        forward: !c.forward,
        speed_factor: c.speed_factor
    };
};
var revDirCubes = function (i) {
    return function (state) {
        return {
            cubes: Data_Array.mapWithIndex(function (index) {
                return function (cube) {
                    var $60 = index === i;
                    if ($60) {
                        return revDir(cube);
                    };
                    return cube;
                };
            })(state.cubes)
        };
    };
};

//-------------------------------------------------------------------------------------
var renderView = function (state) {
    var renderButton = function (label) {
        return function (query) {
            return Halogen_HTML_Elements.button([ Halogen_HTML_Properties.title(label), Halogen_HTML_Events.onClick(function (v) {
                return query;
            }) ])([ Halogen_HTML_Core.text(label) ]);
        };
    };
    
    // parallel projection
var project = function (p) {
        return {
            x: p.x + viewCenter.x,
            y: p.y + viewCenter.y
        };
    };
    var getPoint = function (maybePoint) {
        var $$default = {
            x: 100.0,
            y: 100.0
        };
        return Data_Maybe.fromMaybe($$default)(maybePoint);
    };
    var drawVertex = function (idx) {
        return function (v) {
            return Halogen_Svg_Elements.g([  ])([ Halogen_Svg_Elements.text([ Halogen_Svg_Attributes.x(v.x + 5.0), Halogen_Svg_Attributes.y(v.y - 5.0), Halogen_Svg_Attributes.fill(new Data_Maybe.Just(new Halogen_Svg_Attributes.RGB(150, 150, 150))) ])([ Halogen_HTML_Core.text(Data_Show.show(Data_Show.showInt)(idx)) ]), Halogen_Svg_Elements.circle([ Halogen_Svg_Attributes.r(3.0), Halogen_Svg_Attributes.cx(v.x), Halogen_Svg_Attributes.cy(v.y), Halogen_Svg_Attributes.fill(new Data_Maybe.Just(new Halogen_Svg_Attributes.RGB(100, 100, 100))) ]) ]);
        };
    };
    var drawVertices = function (vert2Ds) {
        return Data_Array.mapWithIndex(drawVertex)(vert2Ds);
    };
    var drawLine = function (a) {
        return function (b) {
            return Halogen_Svg_Elements.line([ Halogen_Svg_Attributes.x1(a.x), Halogen_Svg_Attributes.x2(b.x), Halogen_Svg_Attributes.y1(a.y), Halogen_Svg_Attributes.y2(b.y), Halogen_Svg_Attributes.stroke(new Data_Maybe.Just(new Halogen_Svg_Attributes.RGB(50, 50, 50))) ]);
        };
    };
    var drawEdges = function (edges) {
        return function (verts) {
            var connectedVerts = Data_Functor.map(Data_Functor.functorArray)(function (v) {
                return new Data_Tuple.Tuple(Data_Array.index(verts)(v.value0), Data_Array.index(verts)(v.value1));
            })(edges);
            return Data_Functor.map(Data_Functor.functorArray)(function (v) {
                return drawLine(getPoint(v.value0))(getPoint(v.value1));
            })(connectedVerts);
        };
    };
    var drawCube = function (edges) {
        return function (vert2Ds) {
            return Data_Semigroup.append(Data_Semigroup.semigroupArray)(drawEdges(edges)(vert2Ds))(drawVertices(vert2Ds));
        };
    };
    var drawCubes = function (i) {
        return function (cube) {
            var vert2Ds = Data_Functor.map(Data_Functor.functorArray)(project)(cube.shape.vertices);
            return Halogen_HTML_Elements.div([  ])(Data_Semigroup.append(Data_Semigroup.semigroupArray)([ renderButton("Reverse")(new Reversedirection(i)), renderButton("Inc Vel")(new IncVelocity(i)), renderButton("Dec Vel")(new DecVelocity(i)) ])([ Halogen_Svg_Elements.svg([ Halogen_Svg_Attributes.viewBox(0.0)(0.0)(viewBoxSize)(viewBoxSize) ])([ Halogen_Svg_Elements.g([  ])(drawCube(cube.shape.edges)(vert2Ds)) ]) ]));
        };
    };
    return Halogen_HTML_Elements.div([  ])(Data_Semigroup.append(Data_Semigroup.semigroupArray)([ renderButton("rotX++")(new IncAngVelocity(X.value)), renderButton("rotY++")(new IncAngVelocity(Y.value)), renderButton("rotZ++")(new IncAngVelocity(Z.value)), renderButton("AddCube")(Add.value), renderButton("DeleteCube")(Del.value) ])(Data_Array.mapWithIndex(drawCubes)(state.cubes)));
};
var oneDegInRad = 1.745329255e-2;
var tenDegInRad = oneDegInRad * 10.0;
var initCube = {
    shape: {
        vertices: [ {
            x: 100.0,
            y: 100.0,
            z: 100.0
        }, {
            x: -100.0,
            y: 100.0,
            z: 100.0
        }, {
            x: 100.0,
            y: -100.0,
            z: 100.0
        }, {
            x: -100.0,
            y: -100.0,
            z: 100.0
        }, {
            x: 100.0,
            y: 100.0,
            z: -100.0
        }, {
            x: -100.0,
            y: 100.0,
            z: -100.0
        }, {
            x: 100.0,
            y: -100.0,
            z: -100.0
        }, {
            x: -100.0,
            y: -100.0,
            z: -100.0
        } ],
        edges: [ new Data_Tuple.Tuple(0, 1), new Data_Tuple.Tuple(0, 2), new Data_Tuple.Tuple(0, 4), new Data_Tuple.Tuple(1, 5), new Data_Tuple.Tuple(1, 3), new Data_Tuple.Tuple(2, 3), new Data_Tuple.Tuple(2, 6), new Data_Tuple.Tuple(4, 5), new Data_Tuple.Tuple(4, 6), new Data_Tuple.Tuple(3, 7), new Data_Tuple.Tuple(6, 7), new Data_Tuple.Tuple(5, 7) ]
    },
    angVel: {
        xa: tenDegInRad,
        ya: tenDegInRad,
        za: tenDegInRad
    },
    forward: true,
    speed_factor: 1.0
};
var incVel = function (c) {
    return {
        shape: c.shape,
        angVel: c.angVel,
        forward: c.forward,
        speed_factor: c.speed_factor * 2.0
    };
};
var incVelCubes = function (i) {
    return function (state) {
        return {
            cubes: Data_Array.mapWithIndex(function (index) {
                return function (cube) {
                    var $74 = index === i;
                    if ($74) {
                        return incVel(cube);
                    };
                    return cube;
                };
            })(state.cubes)
        };
    };
};
var frameRate = 200.0;
var delCube = function (state) {
    var cubes1 = Data_Array.tail(state.cubes);
    if (cubes1 instanceof Data_Maybe.Just) {
        var $76 = {};
        for (var $77 in state) {
            if ({}.hasOwnProperty.call(state, $77)) {
                $76[$77] = state[$77];
            };
        };
        $76.cubes = cubes1.value0;
        return $76;
    };
    var $80 = {};
    for (var $81 in state) {
        if ({}.hasOwnProperty.call(state, $81)) {
            $80[$81] = state[$81];
        };
    };
    $80.cubes = [  ];
    return $80;
};
var decVel = function (c) {
    return {
        shape: c.shape,
        angVel: c.angVel,
        forward: c.forward,
        speed_factor: c.speed_factor / 2.0
    };
};
var decVelCubes = function (i) {
    return function (state) {
        return {
            cubes: Data_Array.mapWithIndex(function (index) {
                return function (cube) {
                    var $83 = index === i;
                    if ($83) {
                        return decVel(cube);
                    };
                    return cube;
                };
            })(state.cubes)
        };
    };
};
var dampenPercent = 1.0 - 0.9 / frameRate;
var dampenAngVelocity = function (v) {
    var dampen = function (ang) {
        return ang * dampenPercent;
    };
    return {
        xa: dampen(v.xa),
        ya: dampen(v.ya),
        za: dampen(v.za)
    };
};
var anglePerFrame = function (v) {
    return {
        xa: v.xa / frameRate,
        ya: v.ya / frameRate,
        za: v.za / frameRate
    };
};

// decAngVelocity :: Axis -> Cube -> Cube
// decAngVelocity axis c = do 
//   let {xa, ya, za} = c.angVel
//   case axis of
//     X -> c { angVel { xa = xa - accelerateBy } }
//     Y -> c { angVel { ya = ya - accelerateBy } }
//     Z -> c { angVel { za = za - accelerateBy } }
var tick = function (c) {
    var newShape = {
        edges: c.shape.edges,
        vertices: rotateShape(c.shape.vertices)(anglePerFrame(c.angVel))
    };
    var newCube = {
        angVel: dampenAngVelocity(c.angVel),
        shape: newShape,
        forward: c.forward,
        speed_factor: c.speed_factor
    };
    return newCube;
};
var tickCubes = function (state) {
    return {
        cubes: Data_Functor.map(Data_Functor.functorArray)(tick)(state.cubes)
    };
};
var addCube = function (state) {
    return {
        cubes: Data_Semigroup.append(Data_Semigroup.semigroupArray)(state.cubes)([ initCube ])
    };
};
var accelerateBy = oneDegInRad * 50.0;
var incAngVelocityCube = function (axis) {
    return function (c) {
        var $96 = c.forward === true;
        if ($96) {
            if (axis instanceof X) {
                return {
                    shape: c.shape,
                    angVel: {
                        xa: c.angVel.xa + accelerateBy * c.speed_factor,
                        ya: c.angVel.ya,
                        za: c.angVel.za
                    },
                    forward: c.forward,
                    speed_factor: c.speed_factor
                };
            };
            if (axis instanceof Y) {
                return {
                    shape: c.shape,
                    angVel: {
                        xa: c.angVel.xa,
                        ya: c.angVel.ya + accelerateBy * c.speed_factor,
                        za: c.angVel.za
                    },
                    forward: c.forward,
                    speed_factor: c.speed_factor
                };
            };
            if (axis instanceof Z) {
                return {
                    shape: c.shape,
                    angVel: {
                        xa: c.angVel.xa,
                        ya: c.angVel.ya,
                        za: c.angVel.za + accelerateBy * c.speed_factor
                    },
                    forward: c.forward,
                    speed_factor: c.speed_factor
                };
            };
            throw new Error("Failed pattern match at Cube (line 259, column 5 - line 262, column 68): " + [ axis.constructor.name ]);
        };
        if (axis instanceof X) {
            return {
                shape: c.shape,
                angVel: {
                    xa: c.angVel.xa - accelerateBy * c.speed_factor,
                    ya: c.angVel.ya,
                    za: c.angVel.za
                },
                forward: c.forward,
                speed_factor: c.speed_factor
            };
        };
        if (axis instanceof Y) {
            return {
                shape: c.shape,
                angVel: {
                    xa: c.angVel.xa,
                    ya: c.angVel.ya - accelerateBy * c.speed_factor,
                    za: c.angVel.za
                },
                forward: c.forward,
                speed_factor: c.speed_factor
            };
        };
        if (axis instanceof Z) {
            return {
                shape: c.shape,
                angVel: {
                    xa: c.angVel.xa,
                    ya: c.angVel.ya,
                    za: c.angVel.za - accelerateBy * c.speed_factor
                },
                forward: c.forward,
                speed_factor: c.speed_factor
            };
        };
        throw new Error("Failed pattern match at Cube (line 263, column 8 - line 266, column 68): " + [ axis.constructor.name ]);
    };
};
var incAngVelocity = function (axis) {
    return function (cubes1) {
        return Data_Functor.map(Data_Functor.functorArray)(incAngVelocityCube(axis))(cubes1);
    };
};
var cubes = (function () {
    var runFunction = function (fn) {
        return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.modify(Halogen_Query_HalogenM.monadStateHalogenM)(fn))(function () {
            return Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(Data_Unit.unit);
        });
    };
    var initialState = {
        cubes: [ initCube ]
    };
    var handleQuery = function (v) {
        if (v instanceof Tick) {
            return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.modify(Halogen_Query_HalogenM.monadStateHalogenM)(function (c) {
                return tickCubes(c);
            }))(function () {
                return Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(new Data_Maybe.Just(v.value0));
            });
        };
        if (v instanceof Other) {
            return Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(new Data_Maybe.Just(v.value0));
        };
        throw new Error("Failed pattern match at Cube (line 209, column 23 - line 214, column 26): " + [ v.constructor.name ]);
    };
    var handleAction = function (query) {
        if (query instanceof DecAngVelocity) {
            return Control_Monad_State_Class.modify_(Halogen_Query_HalogenM.monadStateHalogenM)(function (state) {
                return state;
            });
        };
        if (query instanceof IncAngVelocity) {
            return runFunction(function (c) {
                return {
                    cubes: incAngVelocity(query.value0)(c.cubes)
                };
            });
        };
        if (query instanceof Reversedirection) {
            return runFunction(revDirCubes(query.value0));
        };
        if (query instanceof IncVelocity) {
            return runFunction(incVelCubes(query.value0));
        };
        if (query instanceof DecVelocity) {
            return runFunction(decVelCubes(query.value0));
        };
        if (query instanceof Add) {
            return runFunction(addCube);
        };
        if (query instanceof Del) {
            return runFunction(delCube);
        };
        throw new Error("Failed pattern match at Cube (line 199, column 30 - line 206, column 41): " + [ query.constructor.name ]);
    };
    return Halogen_Component.mkComponent({
        initialState: Data_Function["const"](initialState),
        render: renderView,
        "eval": Halogen_Component.mkEval({
            handleAction: handleAction,
            handleQuery: handleQuery,
            receive: Halogen_Component.defaultEval.receive,
            initialize: Halogen_Component.defaultEval.initialize,
            finalize: Halogen_Component.defaultEval.finalize
        })
    });
})();
module.exports = {
    DecAngVelocity: DecAngVelocity,
    IncAngVelocity: IncAngVelocity,
    Reversedirection: Reversedirection,
    IncVelocity: IncVelocity,
    DecVelocity: DecVelocity,
    Add: Add,
    Del: Del,
    Tick: Tick,
    Other: Other,
    accelerateBy: accelerateBy,
    anglePerFrame: anglePerFrame,
    cubes: cubes,
    dampenAngVelocity: dampenAngVelocity,
    dampenPercent: dampenPercent,
    decVel: decVel,
    frameRate: frameRate,
    incAngVelocity: incAngVelocity,
    incVel: incVel,
    initCube: initCube,
    oneDegInRad: oneDegInRad,
    renderView: renderView,
    revDir: revDir,
    rotate: rotate,
    rotateShape: rotateShape,
    tenDegInRad: tenDegInRad,
    tick: tick,
    viewBoxSize: viewBoxSize,
    viewCenter: viewCenter
};
