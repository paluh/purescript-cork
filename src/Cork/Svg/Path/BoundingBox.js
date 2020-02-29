var getBounds = require('svg-path-bounds');

exports.boundingBox = function(path) {
  var bb = getBounds(path);
  return {
    x: bb[0],
    y: bb[1],
    width: bb[3],
    height: bb[4]
  };
};
