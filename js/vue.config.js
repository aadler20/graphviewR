module.exports = {
  publicPath:
    process.env.NODE_ENV == "production" ? process.env.CONNECT_URL : "/",
  outputDir: "../files/static2",
  devServer: {
    proxy: process.env.PROXY_URL
  }
};
