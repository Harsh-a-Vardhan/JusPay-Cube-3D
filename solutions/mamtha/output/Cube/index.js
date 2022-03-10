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
var ReverseCube = (function () {
    function ReverseCube(value0) {
        this.value0 = value0;
    };
    ReverseCube.create = function (value0) {
        return new ReverseCube(value0);
    };
    return ReverseCube;
})();

// Events
var IncreaseSpeed = (function () {
    function IncreaseSpeed(value0) {
        this.value0 = value0;
    };
    IncreaseSpeed.create = function (value0) {
        return new IncreaseSpeed(value0);
    };
    return IncreaseSpeed;
})();

// Events
var DecreaseSpeed = (function () {
    function DecreaseSpeed(value0) {
        this.value0 = value0;
    };
    DecreaseSpeed.create = function (value0) {
        return new DecreaseSpeed(value0);
    };
    return DecreaseSpeed;
})();

// Events
var AddCube = (function () {
    function AddCube() {

    };
    AddCube.value = new AddCube();
    return AddCube;
})();

// Events
var DeleteCube = (function () {
    function DeleteCube() {

    };
    DeleteCube.value = new DeleteCube();
    return DeleteCube;
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
    var $96 = rotateZ(v.za);
    var $97 = rotateY(v.ya);
    var $98 = rotateX(v.xa);
    return function ($99) {
        return $96($97($98($99)));
    };
};
var rotateShape = function (vertices) {
    return function (ang) {
        return Data_Functor.map(Data_Functor.functorArray)(rotate(ang))(vertices);
    };
};
var reverseCube = function (id) {
    return function (c) {
        var $51 = id === c.id;
        if ($51) {
            return {
                shape: c.shape,
                angVel: c.angVel,
                forward: !c.forward,
                speedControl: c.speedControl,
                id: c.id
            };
        };
        return c;
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
    var vert2Ds = Data_Functor.map(Data_Functor.functorArray)(project)(state.shape.vertices);
    return Halogen_HTML_Elements.div([  ])(Data_Semigroup.append(Data_Semigroup.semigroupArray)([ renderButton("rotX++")(new IncAngVelocity(X.value)), renderButton("rotY++")(new IncAngVelocity(Y.value)), renderButton("rotZ++")(new IncAngVelocity(Z.value)), renderButton("reverse")(new ReverseCube(state.id)), renderButton("Vel++")(new IncreaseSpeed(state.id)), renderButton("Vel--")(new DecreaseSpeed(state.id)), renderButton("AddCube")(AddCube.value), renderButton("DeleteCube")(DeleteCube.value), Halogen_HTML_Core.text("  Id: " + (Data_Show.show(Data_Show.showInt)(state.id) + (" Direction: " + (Data_Show.show(Data_Show.showBoolean)(state.forward) + (" Speed: " + Data_Show.show(Data_Show.showNumber)(state.speedControl)))))) ])([ Halogen_Svg_Elements.svg([ Halogen_Svg_Attributes.viewBox(0.0)(0.0)(viewBoxSize)(viewBoxSize) ])([ Halogen_Svg_Elements.g([  ])(drawCube(state.shape.edges)(vert2Ds)) ]) ]));
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
    speedControl: 1.0,
    id: 0
};
var increaseSpeed = function (id) {
    return function (c) {
        var $65 = id === c.id;
        if ($65) {
            return {
                shape: c.shape,
                angVel: c.angVel,
                forward: c.forward,
                speedControl: c.speedControl * 2.0,
                id: c.id
            };
        };
        return c;
    };
};
var frameRate = 200.0;
var decreaseSpeed = function (id) {
    return function (c) {
        var $66 = id === c.id;
        if ($66) {
            return {
                shape: c.shape,
                angVel: c.angVel,
                forward: c.forward,
                speedControl: c.speedControl / 2.0,
                id: c.id
            };
        };
        return c;
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

/**
 *  addCube :: Cube -> Cube
 * addCube  CubeArray c
 */
var tick = function (c) {
    var newShape = {
        edges: c.shape.edges,
        vertices: rotateShape(c.shape.vertices)(anglePerFrame(c.angVel))
    };
    var newCube = {
        angVel: dampenAngVelocity(c.angVel),
        shape: newShape,
        forward: c.forward,
        id: c.id,
        speedControl: c.speedControl
    };
    return newCube;
};
var accelerateBy = oneDegInRad * 50.0;
var incAngVelocity = function (axis) {
    return function (c) {
        if (axis instanceof X) {
            return {
                shape: c.shape,
                angVel: {
                    xa: (function () {
                        if (c.forward) {
                            return c.angVel.xa + accelerateBy * c.speedControl;
                        };
                        return c.angVel.xa - accelerateBy * c.speedControl;
                    })(),
                    ya: c.angVel.ya,
                    za: c.angVel.za
                },
                forward: c.forward,
                speedControl: c.speedControl,
                id: c.id
            };
        };
        if (axis instanceof Y) {
            return {
                shape: c.shape,
                angVel: {
                    xa: c.angVel.xa,
                    ya: (function () {
                        if (c.forward) {
                            return c.angVel.ya + accelerateBy * c.speedControl;
                        };
                        return c.angVel.ya - accelerateBy * c.speedControl;
                    })(),
                    za: c.angVel.za
                },
                forward: c.forward,
                speedControl: c.speedControl,
                id: c.id
            };
        };
        if (axis instanceof Z) {
            return {
                shape: c.shape,
                angVel: {
                    xa: c.angVel.xa,
                    ya: c.angVel.ya,
                    za: (function () {
                        if (c.forward) {
                            return c.angVel.za + accelerateBy * c.speedControl;
                        };
                        return c.angVel.za - accelerateBy * c.speedControl;
                    })()
                },
                forward: c.forward,
                speedControl: c.speedControl,
                id: c.id
            };
        };
        throw new Error("Failed pattern match at Cube (line 215, column 3 - line 218, column 128): " + [ axis.constructor.name ]);
    };
};
var cubes = (function () {
    var runFunction = function (fn) {
        return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.modify(Halogen_Query_HalogenM.monadStateHalogenM)(fn))(function () {
            return Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(Data_Unit.unit);
        });
    };
    var render = function (state) {
        return Halogen_HTML_Elements.div([  ])([ Halogen_HTML_Elements.ul([  ])(Data_Functor.map(Data_Functor.functorArray)(renderView)(state)) ]);
    };
    var initialState = [ initCube ];
    var handleQuery = function (v) {
        if (v instanceof Tick) {
            return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.modify(Halogen_Query_HalogenM.monadStateHalogenM)(function (c) {
                return Data_Functor.map(Data_Functor.functorArray)(tick)(c);
            }))(function () {
                return Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(new Data_Maybe.Just(v.value0));
            });
        };
        if (v instanceof Other) {
            return Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(new Data_Maybe.Just(v.value0));
        };
        throw new Error("Failed pattern match at Cube (line 204, column 23 - line 209, column 26): " + [ v.constructor.name ]);
    };
    var handleAction = function (query) {
        if (query instanceof DecAngVelocity) {
            return Control_Monad_State_Class.modify_(Halogen_Query_HalogenM.monadStateHalogenM)(function (state) {
                return state;
            });
        };
        if (query instanceof IncAngVelocity) {
            return runFunction(function (c) {
                return Data_Functor.map(Data_Functor.functorArray)(incAngVelocity(query.value0))(c);
            });
        };
        if (query instanceof ReverseCube) {
            return runFunction(function (c) {
                return Data_Functor.map(Data_Functor.functorArray)(reverseCube(query.value0))(c);
            });
        };
        if (query instanceof IncreaseSpeed) {
            return runFunction(function (c) {
                return Data_Functor.map(Data_Functor.functorArray)(increaseSpeed(query.value0))(c);
            });
        };
        if (query instanceof DecreaseSpeed) {
            return runFunction(function (c) {
                return Data_Functor.map(Data_Functor.functorArray)(decreaseSpeed(query.value0))(c);
            });
        };
        if (query instanceof AddCube) {
            return runFunction(function (c) {
                return Data_Array.snoc(c)({
                    shape: initCube.shape,
                    angVel: initCube.angVel,
                    forward: initCube.forward,
                    speedControl: initCube.speedControl,
                    id: Data_Array.length(c)
                });
            });
        };
        if (query instanceof DeleteCube) {
            return runFunction(function (c) {
                var $95 = Data_Array.length(c) > 1;
                if ($95) {
                    return Data_Array.dropEnd(1)(c);
                };
                return c;
            });
        };
        throw new Error("Failed pattern match at Cube (line 194, column 30 - line 201, column 86): " + [ query.constructor.name ]);
    };
    return Halogen_Component.mkComponent({
        initialState: Data_Function["const"](initialState),
        render: render,
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
    ReverseCube: ReverseCube,
    IncreaseSpeed: IncreaseSpeed,
    DecreaseSpeed: DecreaseSpeed,
    AddCube: AddCube,
    DeleteCube: DeleteCube,
    X: X,
    Y: Y,
    Z: Z,
    Tick: Tick,
    Other: Other,
    accelerateBy: accelerateBy,
    anglePerFrame: anglePerFrame,
    cubes: cubes,
    dampenAngVelocity: dampenAngVelocity,
    dampenPercent: dampenPercent,
    frameRate: frameRate,
    incAngVelocity: incAngVelocity,
    increaseSpeed: increaseSpeed,
    initCube: initCube,
    oneDegInRad: oneDegInRad,
    renderView: renderView,
    reverseCube: reverseCube,
    rotate: rotate,
    rotateShape: rotateShape,
    tenDegInRad: tenDegInRad,
    tick: tick,
    viewBoxSize: viewBoxSize,
    viewCenter: viewCenter
};
