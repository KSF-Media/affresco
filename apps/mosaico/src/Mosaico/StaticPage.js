export function getInitialStaticPageContent() {
    const staticContainer = document.querySelector('#app .mosaico--static-page');
    return staticContainer ? staticContainer.innerHTML : null;
}

export function getInitialStaticPageScript() {
    const scriptContainer = document.querySelector('#app .mosaico--static-page script');
    return scriptContainer ? scriptContainer.innerHTML : null;
}
