// Generated by purs version 0.14.7
"use strict";
var Data_Enum_Gen = require("../Data.Enum.Gen/index.js");
var Data_Time_Component = require("../Data.Time.Component/index.js");
var genSecond = function (dictMonadGen) {
    return Data_Enum_Gen.genBoundedEnum(dictMonadGen)(Data_Time_Component.boundedEnumSecond);
};
var genMinute = function (dictMonadGen) {
    return Data_Enum_Gen.genBoundedEnum(dictMonadGen)(Data_Time_Component.boundedEnumMinute);
};
var genMillisecond = function (dictMonadGen) {
    return Data_Enum_Gen.genBoundedEnum(dictMonadGen)(Data_Time_Component.boundedEnumMillisecond);
};
var genHour = function (dictMonadGen) {
    return Data_Enum_Gen.genBoundedEnum(dictMonadGen)(Data_Time_Component.boundedEnumHour);
};
module.exports = {
    genHour: genHour,
    genMinute: genMinute,
    genSecond: genSecond,
    genMillisecond: genMillisecond
};
