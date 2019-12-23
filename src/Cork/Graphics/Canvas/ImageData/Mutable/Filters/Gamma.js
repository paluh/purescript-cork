"use strict";
/* global exports, require, Uint8Array */
/* jshint -W097 */
Object.defineProperty(exports, "__esModule", { value: true });
// gamma values should be 0.01 to 2.2.
exports.filter = function (gamma) {
    var rVals = new Uint8Array(256);
    var gVals = new Uint8Array(256);
    var bVals = new Uint8Array(256);
    var rInv = 1 / gamma.r;
    var gInv = 1 / gamma.g;
    var bInv = 1 / gamma.b;
    var i, len;
    for (i = 0, len = 256; i < len; i++) {
        rVals[i] = Math.pow(i / 255, rInv) * 255;
        gVals[i] = Math.pow(i / 255, gInv) * 255;
        bVals[i] = Math.pow(i / 255, bInv) * 255;
    }
    return function (imageData) {
        return function () {
            var data = imageData.data;
            var len = data.length;
            for (i = 0; i < len; i += 4) {
                data[i] = rVals[data[i]];
                data[i + 1] = gVals[data[i + 1]];
                data[i + 2] = bVals[data[i + 2]];
            }
        };
    };
};
//# sourceMappingURL=Gamma.js.map