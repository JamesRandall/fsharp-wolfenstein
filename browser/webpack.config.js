// Note this only includes basic configuration for development mode.
// For a more comprehensive configuration check:
// https://github.com/fable-compiler/webpack-config-template

var path = require("path");
var isProduction = !process.argv.find(v => v.indexOf('webpack-dev-server') !== -1);

module.exports = {
    mode: "development",
    entry: "./src/App.fs.js",
    devtool: isProduction ? "source-map" : "eval-source-map",
    output: {
        path: resolve(__dirname, "./public"),
        filename: "bundle.js",
    },
    devServer: {
        publicPath: "/",
        contentBase: "./public",
        port: 8080,
    },
    module: {
        rules: [
            {
                test: /\.js$/,
                enforce: "pre",
                use: ["source-map-loader"],
            }
        ]
    }
}

function resolve(filePath) {
    return path.isAbsolute(filePath) ? filePath : path.join(__dirname, filePath);
}