/* global exports, require, window, self */
/* jshint -W097 */

"use strict";

exports.selfUnsafeCreateImageBitmapImpl = function() {
  if(!('createImageBitmap' in (window || self))) {
    return null;
  }
  return function(arg) {
    return (window || self).createImageBitmap(arg);
  };
};

exports["height'"] = function(img) {
  return img.height;
};

exports["width'"] = function(img) {
  return img.width;
};
