/* global document, exports, require, window */
/* jshint -W097 */

"use strict";

var Example = require('../output/Example/index');
var domready = require('domready');
domready(function () {
  Example.main();
});
