/* global exports, require */
/* jshint -W097 */

"use strict";

exports.setStyleProperty = function(property) {
  return function(value) {
    return function(elem) {
      return function() {
        elem.style[property] = value;
      };
    };
  };
};
