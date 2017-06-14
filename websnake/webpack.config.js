const path = require('path');

module.exports = {
  entry: {
    websnake: './lib/js/src/websnake/app.js'
  },
  output: {
    path: path.join(__dirname, "bundledOutputs"),
    filename: '[name].js',
  },
};
