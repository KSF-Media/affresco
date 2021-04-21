import Cookies from 'js-cookie';

export const isUserLoggedIn = () => {
    return !!Cookies.get('LoginStatus');
};

export const shareArticle = (title, url, description) => {
    return AndroidNativeShare(title, url, description);
};

export const getUrlParam =() => {return new URLSearchParams(window.location.search)};

export const getBrandValueParam = () => {
    let urlParams = getUrlParam();
    if (urlParams.has('paper')) {
       return urlParams.get('paper')
    }
    return 'hbl'
}
export const getMode = () => {
    let urlParams = getUrlParam();
    if (urlParams.has('mode') && urlParams.get('mode') == 'dark') {
        return 'dark'
    }

    return 'normal'
}

export const isDarkModeOn = () => {
    let urlParams = getUrlParam();
    if (urlParams.has('mode') && urlParams.get('mode') == 'dark') {
        return true
    }
    return false
}

export const  checkIfUserIsLoggedInfromUrl = () => {
    let urlParams = getUrlParam();
    if (urlParams.has('islogged') && urlParams.get('islogged') == 'true') {
        return true
    }
    return false
}

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

export function updateURLParameter(url, param, paramVal)
{
    var TheAnchor = null;
    var newAdditionalURL = "";
    var tempArray = url.split("?");
    var baseURL = tempArray[0];
    var additionalURL = tempArray[1];
    var temp = "";

    if (additionalURL) 
    {
        var tmpAnchor = additionalURL.split("#");
        var TheParams = tmpAnchor[0];
            TheAnchor = tmpAnchor[1];
        if(TheAnchor)
            additionalURL = TheParams;

        tempArray = additionalURL.split("&");

        for (var i=0; i<tempArray.length; i++)
        {
            if(tempArray[i].split('=')[0] != param)
            {
                newAdditionalURL += temp + tempArray[i];
                temp = "&";
            }
        }        
    }
    else
    {
        var tmpAnchor = baseURL.split("#");
        var TheParams = tmpAnchor[0];
            TheAnchor  = tmpAnchor[1];

        if(TheParams)
            baseURL = TheParams;
    }

    if(TheAnchor)
        paramVal += "#" + TheAnchor;

    var rows_txt = temp + "" + param + "=" + paramVal;
    return baseURL + "?" + newAdditionalURL + rows_txt;
}