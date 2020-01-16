/* global exports, require */
/* jshint esversion: 6 */
/* jshint -W097 */

"use strict";

exports.crispy = function(i) {
  return function() {
    var idx;
    var original = i.originalData.data;
    var image = i.imageData.data;
    for(idx = 3; idx < original.length; idx += 4) {
      if(image[idx] !== 0 && image[idx] !== 255 && original[idx] !== image[idx]) {
        image[idx] = 0;
      }
    }
  };
};
