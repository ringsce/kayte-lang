const ffi = require('ffi-napi');
const ref = require('ref-napi');

// path is relative to dist/ after build — adjust if kayte-core.node moves
const kayteLib = ffi.Library('./dist/kayte-core', {
  'runKayteCode': ['void', ['string']],
  'compileKayteCode': ['string', ['string']]
});

function runKayteCode(code) {
  kayteLib.runKayteCode(code);
}

function compileKayteCode(code) {
  return kayteLib.compileKayteCode(code);
}

module.exports = { runKayteCode, compileKayteCode };

