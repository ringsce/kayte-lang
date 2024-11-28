// kayte-js-bindings.js
const ffi = require('ffi-napi');
const ref = require('ref-napi');

// Load the compiled Kayte Lang module (replace 'dist/kayte-core.node' with your path)
const kayteLib = ffi.Library('./dist/kayte-core', {
  'runKayteCode': ['void', ['string']],
  'compileKayteCode': ['string', ['string']]
});

// Run a piece of Kayte Lang code
function runKayteCode(code) {
  kayteLib.runKayteCode(code);
}

// Compile Kayte Lang code to bytecode
function compileKayteCode(code) {
  return kayteLib.compileKayteCode(code);
}

module.exports = { runKayteCode, compileKayteCode };

