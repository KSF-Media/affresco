export const isUserLoggedIn = () => {
    return !(localStorage.getItem("uuid") === null && localStorage.getItem("token") === null);
};

export const shareArticle = (title, url, description) => {
    return AndroidNativeShare(title, url, description);
};

async function AndroidNativeShare(Title, URL, Description) {
    if (typeof navigator.share === 'undefined' || !navigator.share) {
        // alert('Your browser does not support Android Native Share, it\'s tested on chrome 63+');
    } else if (window.location.protocol != 'https:') {
        alert('Android Native Share support only on Https:// protocol');
    } else {
        if (typeof URL === 'undefined') {
            URL = window.location.href;
        }
        if (typeof Title === 'undefined') {
            Title = document.title;
        }
        if (typeof Description === 'undefined') {
            Description = 'Share your thoughts about ' + Title;
        }
        const TitleConst = Title;
        const URLConst = URL;
        const DescriptionConst = Description;

        try {
            await navigator.share({title: TitleConst, text: DescriptionConst, url: URLConst});
        } catch (error) {
            console.log('Error sharing: ' + error);
            return;
        }
    }
}
