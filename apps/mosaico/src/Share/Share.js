exports.encodeURIComponent = function (s) {
  return encodeURIComponent(s);
};

exports.nativeShare = (typeof window !== "undefined" && window.navigator && window.navigator.share)
  ? (data) => {
      window.navigator.share(data);
    }
  : null;
