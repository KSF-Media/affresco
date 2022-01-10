exports.getInitialStaticPageContent = function() {
    const staticContainer = document.querySelector('#app .mosaico--static-page');
    return staticContainer ? staticContainer.innerHTML : null;
}

exports.getInitialStaticPageScript = function() {
    const scriptContainer = document.querySelector('#app .mosaico--static-page script');
    return scriptContainer ? scriptContainer.innerHTML : null;
}
