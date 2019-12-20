/* global exports, ImageData, require, document, window, Uint8ClampedArray */
/* jshint -W097 */

"use strict";

var copyImpl = function(img) {
  return function () {
    return new ImageData(new Uint8ClampedArray(img.data), img.width, img.height);
  };
};

exports.freeze = copyImpl;

exports.thaw = copyImpl;

exports.unsafeFreeze = function(img) {
  return function() {
    return img;
  };
};

exports.unsafeThaw = function(img) {
  return function() {
    return img;
  };
};

exports.height = function(img) {
  return img.height;
};

exports.width = function(img) {
  return img.width;
};

exports.new = function(dims) {
  return function() {
    return new ImageData(new Uint8ClampedArray(dims.width * dims.height * 4), dims.width, dims.height);
  };
};


