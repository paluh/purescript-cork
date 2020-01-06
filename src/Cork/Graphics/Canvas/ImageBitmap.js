/* global exports, require */
/* jshint -W097 */

"use strict";

var Matrix = require('node-matrices');


// https://beta.observablehq.com/@shaunlebron/texture-drawing-for-html-canvas
exports.fillQuadTexImpl = function(ctx, src, dst, tiles) {
  tiles = tiles || 10;

  var projectionSrc = exports.forwardProjectionMatrixForPoints([src.first, src.second, src.third, src.fourth]);
  var projectionDst = exports.forwardProjectionMatrixForPoints([dst.first, dst.second, dst.third, dst.fourth]);
  var srcRowCol = function(r, c) { return exports.projectPoint({ x: c / tiles, y: r / tiles }, projectionSrc); };
  var dstRowCol = function(r, c) { return exports.projectPoint({ x: c / tiles, y: r / tiles }, projectionDst); };

  var pad = 0.03; // we add padding to remove tile seams

  var topTri = function(r, c, p) { return [
    /*
      0-----1
       \    |
         \  |  top
           \|
            2
    */
    p(r - pad, c - pad * 2), // extra diagonal padding
    p(r - pad, c + 1 + pad),
    p(r + 1 + pad * 2, c + 1 + pad), // extra diagonal padding
  ]; };
  var botTri = function(r, c, p) { return [
    /*
      2
      |\
      |  \   bottom
      |    \
      1-----0
    */
    p(r + 1 + pad, c + 1 + pad),
    p(r + 1 + pad, c - pad),
    p(r - pad, c - pad),
  ]; };

  // clip to erase the external padding
  ctx.save();
  ctx.beginPath();
  ctx.lineTo(dst.first.x, dst.first.y);
  ctx.lineTo(dst.second.x, dst.second.y);
  ctx.lineTo(dst.third.x, dst.third.y);
  ctx.lineTo(dst.fourth.x, dst.fourth.y);
  ctx.closePath();
  ctx.clip();

  // draw triangles
  for(var r = 0; r < tiles; r++) {
    for(var c = 0; c < tiles; c++) {
      exports.fillTriTex(ctx, topTri(r, c, srcRowCol), topTri(r, c, dstRowCol));
      exports.fillTriTex(ctx, botTri(r, c, srcRowCol), botTri(r, c, dstRowCol));
    }
  }
  ctx.restore();
};

// Per http://graphics.cs.cmu.edu/courses/15-463/2008_fall/Papers/proj.pdf
exports.forwardProjectionMatrixForPoints = function(points) {
  var deltaX1 = points[1].x - points[2].x;
  var deltaX2 = points[3].x - points[2].x;
  var sumX = points[0].x - points[1].x + points[2].x - points[3].x;
  var deltaY1 = points[1].y - points[2].y;
  var deltaY2 = points[3].y - points[2].y;
  var sumY = points[0].y - points[1].y + points[2].y - points[3].y;
  var denominator = new Matrix([deltaX1, deltaX2], [deltaY1, deltaY2]).determinant();
  var g = new Matrix([sumX, deltaX2], [sumY, deltaY2]).determinant() / denominator;
  var h = new Matrix([deltaX1, sumX], [deltaY1, sumY]).determinant() / denominator;
  var a = points[1].x - points[0].x + g * points[1].x;
  var b = points[3].x - points[0].x + h * points[3].x;
  var c = points[0].x;
  var d = points[1].y - points[0].y + g * points[1].y;
  var e = points[3].y - points[0].y + h * points[3].y;
  var f = points[0].y;
  return new Matrix([a, b, c], [d, e, f], [g, h, 1]);
};

exports.projectPoint = function(point, projectionMatrix) {
  var pointMatrix = projectionMatrix.multiply(new Matrix([point.x], [point.y], [1]));
  return {
    x: pointMatrix.get(0, 0) / pointMatrix.get(2, 0),
    y: pointMatrix.get(1, 0) / pointMatrix.get(2, 0),
  };
};

exports.fillTriTex = function(ctx, src, dst) {
  ctx.beginPath();
  ctx.lineTo(dst[0].x, dst[0].y);
  ctx.lineTo(dst[1].x, dst[1].y);
  ctx.lineTo(dst[2].x, dst[2].y);
  // ctx.lineTo(dst[3].x, dst[3].y);
  ctx.closePath();
  var [[x0, y0], [x1, y1], [x2, y2]] = dst.map(function(r) { return [r.x, r.y]; });
  var [[u0, v0], [u1, v1], [u2, v2]] = src.map(function(r) { return [r.x, r.y]; });
  exports.fillTexPath(ctx, x0, y0, x1, y1, x2, y2, u0, v0, u1, v1, u2, v2);
};

// from: https://github.com/mrdoob/three.js/blob/r91/examples/js/renderers/CanvasRenderer.js#L917
// math: http://extremelysatisfactorytotalitarianism.com/blog/?p=2120
exports.fillTexPath = function(ctx, x0, y0, x1, y1, x2, y2, u0, v0, u1, v1, u2, v2) {
  var a, b, c, d, e, f, det, idet;
  x1 -= x0;
  y1 -= y0;
  x2 -= x0;
  y2 -= y0;
  u1 -= u0;
  v1 -= v0;
  u2 -= u0;
  v2 -= v0;
  det = u1 * v2 - u2 * v1;
  if (det === 0) return;
  idet = 1 / det;
  a = (v2 * x1 - v1 * x2) * idet;
  b = (v2 * y1 - v1 * y2) * idet;
  c = (u1 * x2 - u2 * x1) * idet;
  d = (u1 * y2 - u2 * y1) * idet;
  e = x0 - a * u0 - c * v0;
  f = y0 - b * u0 - d * v0;
  ctx.save();
  ctx.transform(a, b, c, d, e, f);
  ctx.fill();
  ctx.restore();
};

