import * as path from 'path';
import HtmlWebpackPlugin from 'html-webpack-plugin';
import webpack from 'webpack';
const { IgnorePlugin } = webpack;
import CopyPlugin from "copy-webpack-plugin";
import HaskellPlugin from "./haskell-plugin.ts";

// path to where cabal puts the wasm.
const WASM_PATH = "dist-newstyle/build/wasm32-wasi/ghc-9.11.20240828/haskell-webgl-0.1.0.0/x/haskell-webgl/opt/build/haskell-webgl/haskell-webgl.wasm";

export default {
  entry: "./src/index.js",
  output: {
    path: path.resolve('./dist'),
    filename: 'bundle.js',
  },
  module: {
    rules: [
      /*{
        test: /.ffi.js/,
        use: [
          {
            loader: path.resolve('./ffi-loader.js'),
          }
        ]
      },*/
      {
        test: /\.tsx?$/,
        use: 'ts-loader',
        exclude: /node_modules/,
      },
      {
        test: /\.css$/i,
        use: ["style-loader", "css-loader"],
      },
    ],
  },
  resolve: {
    extensions: ['.tsx', '.ts', '.js'],
    alias: {
      // Point to the correct location of Main_ffi.js because
      // IgnorePlugin being weird and annoying.
      // See: https://github.com/webpack/webpack/issues/2858
      //'@hs-artifacts': path.resolve("./hs-artifacts"),
    }
  },
  experiments: {
    //asyncWebAssembly: true,
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
    new HtmlWebpackPlugin({
      title: "Haskell WebGL",
      template: "./src/index.html"
    }),
    // In the Main_ffi file it imports node:timers for the deno
    // implementation. However, webpack will see that and panic,
    // so we have to tell it to ignore that. I think it ends up
    // replacing it with something that throws a webpack error,
    // but that's fine because it figures out that it isn't in
    // deno by having an error thrown there anyway.
    new IgnorePlugin({ resourceRegExp: /node:timers/ }),
    // We load wasm using fetch, so we have to just copy the raw
    // wasm file into our output directory.
    /*new CopyPlugin({
      patterns: [{
        from: path.join(__dirname, "./hs-artifacts/Main.wasm")
      }]
    }),*/
    new HaskellPlugin({
      cabalFile: "haskell-webgl.cabal",
      haskellDir: "app",
      debugMode: true,
      wasmPath: path.resolve(WASM_PATH)
    })
  ],
};
