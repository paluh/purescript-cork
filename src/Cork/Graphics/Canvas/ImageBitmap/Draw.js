/* global exports, require */
/* jshint -W097 */

"use strict";

exports.drawImageFull = function(ctx) {
    return function(img) {
        return function(s) {
            return function(d) {
                return function() {
                    ctx.drawImage(img, s.x, s.y, s.width, s.height, d.x, d.y, d.width, d.height);
                };
            };
        };
    };
};
