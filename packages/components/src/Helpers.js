exports.getTextContentFromHTMLString = function(innerHTML) {
    const tmp = document.createElement('div');
    tmp.innerHTML = innerHTML;
    return tmp.textContent;
}
