/* global exports, require */
/* jshint -W097 */

"use strict";

exports.clearRect = function(ctx) {
    return function(r) {
        return function() {
            ctx.clearRect(r.x, r.y, r.width, r.height);
        };
    };
};

exports.clipPath2DImpl = function(ctx, path2D, fillingRule) {
  ctx.clip(path2D, fillingRule);
};

exports.resetTransform = function(ctx) {
  return function() {
    ctx.resetTransform();
  };
};


