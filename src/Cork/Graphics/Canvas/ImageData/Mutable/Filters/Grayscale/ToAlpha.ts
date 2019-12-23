/* global exports, require */
/* jshint -W097 */

type mode = 'average' | 'lightness' | 'luminosity';

export const filterImpl = function(mode: mode) {
  return function(imageData: ImageData) {
    var data = imageData.data, i, len = data.length, value: number;
    switch(mode) {
      case 'average': return function() {
        for (i = 0; i < len; i += 4) {
          value = (data[i] + data[i + 1] + data[i + 2]) / 3;
          data[i + 3] = value;
        }
      }
      case 'lightness': return function() {
        for (i = 0; i < len; i += 4) {
          value = (Math.min(data[i], data[i + 1], data[i + 2]) +
            Math.max(data[i], data[i + 1], data[i + 2])) / 2;
          data[i + 3] = value;
        }
      }
      case 'luminosity': return function() {
        for (i = 0; i < len; i += 4) {
          value = 0.21 * data[i] + 0.72 * data[i + 1] + 0.07 * data[i + 2];
          data[i + 3] = value;
        }
      }
    };
  };
};
