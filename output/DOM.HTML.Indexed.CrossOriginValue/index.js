// Generated by purs version 0.14.7
"use strict";
var Anonymous = (function () {
    function Anonymous() {

    };
    Anonymous.value = new Anonymous();
    return Anonymous;
})();
var UseCredentials = (function () {
    function UseCredentials() {

    };
    UseCredentials.value = new UseCredentials();
    return UseCredentials;
})();
var renderCrossOriginValue = function (v) {
    if (v instanceof Anonymous) {
        return "anonymous";
    };
    if (v instanceof UseCredentials) {
        return "use-credentials";
    };
    throw new Error("Failed pattern match at DOM.HTML.Indexed.CrossOriginValue (line 8, column 26 - line 10, column 38): " + [ v.constructor.name ]);
};
module.exports = {
    Anonymous: Anonymous,
    UseCredentials: UseCredentials,
    renderCrossOriginValue: renderCrossOriginValue
};
