const axios = require("axios");
const convert = require("xml-js");

exports.handler = function (event, context, callback) {
  const ID = event.queryStringParameters.id;
  const URL = `https://feeds.soundcloud.com/users/soundcloud:users:${ID}/sounds.rss`;

  // Send response
  const send = (body) => {
    callback(null, {
      headers: {
        "Access-Control-Allow-Origin": "*",
        "Access-Control-Allow-Headers": "Origin, X-Requested-With, Content-Type, Accept",
      },
      statusCode: 200,
      body: body,
    });
  };

  // Get Podcast
  const getPodcast = () => {
    axios
      .get(URL)
      .then((resp) => send(convert.xml2json(resp.data, { compact: true })))
      .catch((err) => send(err));
  };

  // Run
  if (event.httpMethod === "GET" && ID) {
    getPodcast();
  }
};
