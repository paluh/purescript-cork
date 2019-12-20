"use strict";
/* global exports, require */
/* jshint -W097 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.filter = function (imageData) {
    return function () {
        // var imageData = new Uint8ClampedArray(input.data);
        // var imageData = new ImageData(
        //   new Uint8ClampedArray(input.data),
        //   input.width,
        //   input.height
        // )
        // imageData.set(input.data);
        var data = imageData.data, len = data.length, i, brightness;
        for (i = 0; i < len; i += 4) {
            brightness = 0.34 * data[i] + 0.5 * data[i + 1] + 0.16 * data[i + 2];
            // red
            data[i] = brightness;
            // green
            data[i + 1] = brightness;
            // blue
            data[i + 2] = brightness;
        }
    };
};
//# sourceMappingURL=Grayscale.js.map