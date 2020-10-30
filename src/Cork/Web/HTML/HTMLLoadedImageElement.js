/* global document, exports, Image, window */
/* jshint -W097 */

"use strict";

exports.newImpl = function(src) {
  return function(onError, onSuccess) {
    var img = new Image();
    exports.setSrcImpl(src)(img)(onError, onSuccess);
  };
};

exports.setSrcImpl = function(src) {
  return function(img) {
    return function(onError, onSuccess) {
      img.onload = function() {
        onSuccess(img);
      };
      img.onerror = function(e) {
        onError(e);
      };
      img.src = src;
      return function (cancelError, cancelerError, cancelerSuccess) {
        cancelError();
      };
    };
  };
};

exports.toDataURLImpl = function(dimensions) {
  return function(img) {
    return function() {
      var canvas = document.createElement('canvas');
      canvas.width = dimensions.width;
      canvas.height = dimensions.height;
      var ctx = canvas.getContext('2d');
      ctx.drawImage(img, 0, 0);
      return canvas.toDataURL('image/png', 1.0);
    };
  };
};

exports["naturalDimensions'"] = function(image) {
  return { height: image.naturalHeight, width: image.naturalWidth };
};
