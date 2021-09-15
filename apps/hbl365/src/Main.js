exports.appStore = {
    apple: require('../../../../images/hbl365/apple.png'),
    android: require('../../../../images/hbl365/android.png')
};

exports.logo = require('../../../../images/hbl365/app-icon-1024x1024-pixels.png');

exports.addOnScroll = function() {
    const header = document.getElementsByTagName('header')[0];
    window.onscroll = function(ev) {
	if (window.scrollY > 38) {
	    header.classList.add('tight');
	} else {
	    header.classList.remove('tight');
	}
    }
}
