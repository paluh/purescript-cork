/* global document, exports, Image, window */
/* jshint -W097 */

"use strict";

exports.changeOrientationToMutableImageDataImpl = function(canvas) {
  return function(orientation) {
    return function(imageBitmap) {
      return function() {
        var width = imageBitmap.width;
        var height = imageBitmap.height;
        var newWidth, newHeight;
        var ctx = canvas.getContext("2d");

        if (orientation == 1 || orientation == 3) {
          newWidth = height;
          newHeight = width;
        } else {
          newWidth = width;
          newHeight = height;
        }
        if(canvas.width < newWidth) {
          canvas.width = newWidth;
        }
        if(canvas.height < newHeight) {
          canvas.height = newHeight;
        }

        switch (orientation) {
          case 1: ctx.transform(0, -1, 1, 0, 0, width); break;
          case 2: ctx.transform(-1, 0, 0, -1, width, height); break;
          case 3: ctx.transform(0, 1, -1, 0, height, 0); break;
          default: break;
        }
        ctx.drawImage(imageBitmap, 0, 0);
        return ctx.getImageData(0, 0, newWidth, newHeight);
      };
    };
  };
};

