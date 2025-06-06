// Generated by purs version 0.14.7
"use strict";
var Effect_Class = require("../Effect.Class/index.js");
var Effect_Console = require("../Effect.Console/index.js");
var warnShow = function (dictMonadEffect) {
    return function (dictShow) {
        var $16 = Effect_Class.liftEffect(dictMonadEffect);
        var $17 = Effect_Console.warnShow(dictShow);
        return function ($18) {
            return $16($17($18));
        };
    };
};
var warn = function (dictMonadEffect) {
    var $19 = Effect_Class.liftEffect(dictMonadEffect);
    return function ($20) {
        return $19(Effect_Console.warn($20));
    };
};
var timeLog = function (dictMonadEffect) {
    var $21 = Effect_Class.liftEffect(dictMonadEffect);
    return function ($22) {
        return $21(Effect_Console.timeLog($22));
    };
};
var timeEnd = function (dictMonadEffect) {
    var $23 = Effect_Class.liftEffect(dictMonadEffect);
    return function ($24) {
        return $23(Effect_Console.timeEnd($24));
    };
};
var time = function (dictMonadEffect) {
    var $25 = Effect_Class.liftEffect(dictMonadEffect);
    return function ($26) {
        return $25(Effect_Console.time($26));
    };
};
var logShow = function (dictMonadEffect) {
    return function (dictShow) {
        var $27 = Effect_Class.liftEffect(dictMonadEffect);
        var $28 = Effect_Console.logShow(dictShow);
        return function ($29) {
            return $27($28($29));
        };
    };
};
var log = function (dictMonadEffect) {
    var $30 = Effect_Class.liftEffect(dictMonadEffect);
    return function ($31) {
        return $30(Effect_Console.log($31));
    };
};
var infoShow = function (dictMonadEffect) {
    return function (dictShow) {
        var $32 = Effect_Class.liftEffect(dictMonadEffect);
        var $33 = Effect_Console.infoShow(dictShow);
        return function ($34) {
            return $32($33($34));
        };
    };
};
var info = function (dictMonadEffect) {
    var $35 = Effect_Class.liftEffect(dictMonadEffect);
    return function ($36) {
        return $35(Effect_Console.info($36));
    };
};
var errorShow = function (dictMonadEffect) {
    return function (dictShow) {
        var $37 = Effect_Class.liftEffect(dictMonadEffect);
        var $38 = Effect_Console.errorShow(dictShow);
        return function ($39) {
            return $37($38($39));
        };
    };
};
var error = function (dictMonadEffect) {
    var $40 = Effect_Class.liftEffect(dictMonadEffect);
    return function ($41) {
        return $40(Effect_Console.error($41));
    };
};
var clear = function (dictMonadEffect) {
    return Effect_Class.liftEffect(dictMonadEffect)(Effect_Console.clear);
};
module.exports = {
    log: log,
    logShow: logShow,
    warn: warn,
    warnShow: warnShow,
    error: error,
    errorShow: errorShow,
    info: info,
    infoShow: infoShow,
    time: time,
    timeLog: timeLog,
    timeEnd: timeEnd,
    clear: clear
};
