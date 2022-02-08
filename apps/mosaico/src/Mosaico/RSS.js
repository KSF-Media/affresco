exports.delete = function() {
  const el = document.querySelector('link[type="application/rss+xml"][rel="alternate"]');
  if (el) {
    el.remove();
  }
}

exports.inject = function(content) {
  const head = document.querySelector('head');
  // Highly unusual to not have it but let's play safe
  if (head) {
    head.append(content);
  }
}
