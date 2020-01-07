exports.filter = function(canvas1) {
  return function(canvas2) {
    return function(blur) {
      return function(imageData) {
        return function() {
          var width = imageData.width;
          var height = imageData.height;
          var ctx1 = canvas1.getContext('2d');
          var ctx2 = canvas2.getContext('2d');
          var nSamples = 15;
          var random, percent, j, i;

          blur = blur * 0.06 * 0.5;

          if (!canvas1.width || canvas1.width < width) {
            canvas1.width = width;
          }
          if (!canvas2.width || canvas2.width < width) {
            canvas2.width = width;
          }
          if(!canvas1.height || canvas1.height < height) {
            canvas1.height = height;
          }
          if(!canvas2.height || canvas2.height < height) {
            canvas2.height = height;
          }
          // load first canvas
          ctx1.putImageData(imageData, 0, 0);
          ctx2.clearRect(0, 0, width, height);

          for (i = -nSamples; i <= nSamples; i++) {
            random = 0.0; //(Math.random() - 0.5) / 4;
            percent = i / nSamples;
            j = blur * percent * width + random;
            ctx2.globalAlpha = 1 - Math.abs(percent);
            ctx2.drawImage(canvas1, j, random);
            ctx1.drawImage(canvas2, 0, 0);
            ctx2.globalAlpha = 1;
            ctx2.clearRect(0, 0, width, height);
          }
          for (i = -nSamples; i <= nSamples; i++) {
            random = 0.0; //(Math.random() - 0.5) / 4;
            percent = i / nSamples;
            j = blur * percent * height + random;
            ctx2.globalAlpha = 1 - Math.abs(percent);
            ctx2.drawImage(canvas1, random, j);
            ctx1.drawImage(canvas2, 0, 0);
            ctx2.globalAlpha = 1;
            ctx2.clearRect(0, 0, width, height);
          }

          ctx2.clearRect(0, 0, width, height);
          ctx2.putImageData(imageData, 0, 0);

          ctx1.globalCompositeOperation = 'destination-in';
          ctx1.drawImage(canvas2, 0, 0);
          ctx1.globalCompositeOperation = 'source-over';

          ctx2.drawImage(canvas1, 0, 0);

          var newImageData = ctx2.getImageData(0, 0, width, height);
          ctx1.globalAlpha = 1;

          return newImageData;
        };
      };
    };
  };
};
