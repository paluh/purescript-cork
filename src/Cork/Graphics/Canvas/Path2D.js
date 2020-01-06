/* global exports, Path2D, require */
/* jshint -W097 */

"use strict";

exports.new = function(repr) {
  return function() {
    return new Path2D(repr);
  };
};


