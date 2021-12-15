exports.getInitialStaticPageContent = function() {
    const staticContainer = document.querySelector('#app .mosaico--static-page');
    return staticContainer ? staticContainer.innerHTML : null;
}
