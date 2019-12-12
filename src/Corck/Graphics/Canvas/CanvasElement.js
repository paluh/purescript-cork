/* global document, exports, require */
/* jshint -W097 */

"use strict";

exports.newImpl = function() {
  return document.createElement('canvas');
};

exports.clone = function(oldCanvas) {
  return function() {
    //create a new canvas
    var newCanvas = document.createElement('canvas');
    var context = newCanvas.getContext('2d');

    //set dimensions
    newCanvas.width = oldCanvas.width;
    newCanvas.height = oldCanvas.height;

    //apply the old canvas to the new one
    context.drawImage(oldCanvas, 0, 0);

    //return the new canvas
    return newCanvas;
  };
};


