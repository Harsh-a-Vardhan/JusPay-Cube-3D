// Generated by purs version 0.14.7
"use strict";
var Control_Apply = require("../Control.Apply/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Time = require("../Data.Time/index.js");
var Data_Time_Component_Gen = require("../Data.Time.Component.Gen/index.js");
var genTime = function (dictMonadGen) {
    return Control_Apply.apply(((dictMonadGen.Monad0()).Bind1()).Apply0())(Control_Apply.apply(((dictMonadGen.Monad0()).Bind1()).Apply0())(Control_Apply.apply(((dictMonadGen.Monad0()).Bind1()).Apply0())(Data_Functor.map((((dictMonadGen.Monad0()).Bind1()).Apply0()).Functor0())(Data_Time.Time.create)(Data_Time_Component_Gen.genHour(dictMonadGen)))(Data_Time_Component_Gen.genMinute(dictMonadGen)))(Data_Time_Component_Gen.genSecond(dictMonadGen)))(Data_Time_Component_Gen.genMillisecond(dictMonadGen));
};
module.exports = {
    genTime: genTime,
    genHour: Data_Time_Component_Gen.genHour,
    genMillisecond: Data_Time_Component_Gen.genMillisecond,
    genMinute: Data_Time_Component_Gen.genMinute,
    genSecond: Data_Time_Component_Gen.genSecond
};
