const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const webpack = require('webpack');
const CopyPlugin = require("copy-webpack-plugin");

module.exports = {
  entry: "./src/index.js",
  output: {
    path: path.resolve(__dirname, './dist'),
    filename: 'bundle.js',
  },
   module: {
    rules: [
      {
        test: /\.tsx?$/,
        use: 'ts-loader',
        exclude: /node_modules/,
      },
    ],
  },
  resolve: {
    extensions: ['.tsx', '.ts', '.js'],
    alias: {
      // Point to the correct location of Main_ffi.js because
      // IgnorePlugin being weird and annoying.
      // See: https://github.com/webpack/webpack/issues/2858
      '@hs-artifacts': path.join(__dirname, "./hs-artifacts"),
      //'./Main_ffi.js$': path.join(__dirname, "hs-artifacts/Main_ffi.js"),
      //'./Main.wasm$': path.join(__dirname, "hs-artifacts/Main.wasm"),
    }
  },
  experiments: {
    asyncWebAssembly: true,
    //syncWebAssembly: true
  },
  devServer: {
    devMiddleware: {
      mimeTypes: {
        wasm: 'application/wasm',
      }
    }
  },
  mode: 'development',
  plugins: [
    new HtmlWebpackPlugin(),
    // In the Main_ffi file it imports node:timers for the deno
    // implementation. However, webpack will see that and panic,
    // so we have to tell it to ignore that. I think it ends up
    // replacing it with something that throws a webpack error,
    // but that's fine because it's expected to throw an error
    // anyway.
    new webpack.IgnorePlugin({ resourceRegExp: /node:timers/ }),
    // We load wasm using fetch, so we have to just copy the raw
    //
    new CopyPlugin({
      patterns: [{
        from: path.join(__dirname, "./hs-artifacts/Main.wasm")
      }]
    })
  ]
};
