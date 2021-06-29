import Cookies from "js-cookie";

export const isUserLoggedIn = () => {
  let loginStatusUrl = false;
  if (getIsLoggedFromUrl() === "true") {
    loginStatusUrl = true;
  }

  return !!Cookies.get("LoginStatus") || loginStatusUrl;
};

export const shareArticle = (title, url, description) => {
  return AndroidNativeShare(title, url, description);
};

export const getUrlParam = () => {
  return new URLSearchParams(window.location.search);
};

export const getBrandValueParam = () => {
  let urlParams = getUrlParam();
  if (urlParams.has("paper")) {
    return urlParams.get("paper");
  }
  return "hbl";
};
export const getMode = () => {
  let urlParams = getUrlParam();
  if (urlParams.has("mode") && urlParams.get("mode") == "dark") {
    return "dark";
  }

  return "normal";
};

export const isDarkModeOn = () => {
  let urlParams = getUrlParam();
  if (urlParams.has("mode") && urlParams.get("mode") == "dark") {
    return true;
  }
  return false;
};

async function AndroidNativeShare(Title, URL, Description) {
  if (typeof navigator.share === "undefined" || !navigator.share) {
    // alert('Your browser does not support Android Native Share, it\'s tested on chrome 63+');
  } else if (window.location.protocol != "https:") {
    alert("Android Native Share support only on Https:// protocol");
  } else {
    if (typeof URL === "undefined") {
      URL = window.location.href;
    }
    if (typeof Title === "undefined") {
      Title = document.title;
    }
    if (typeof Description === "undefined") {
      Description = "Share your thoughts about " + Title;
    }
    const TitleConst = Title;
    const URLConst = URL;
    const DescriptionConst = Description;

    try {
      await navigator.share({ title: TitleConst, text: DescriptionConst, url: URLConst });
    } catch (error) {
      console.log("Error sharing: " + error);
      return;
    }
  }
}

export const getTokenFromUrl = () => {
  let urlParams = getUrlParam();
  if (urlParams.has("token")) {
    return urlParams.get("token");
  }

  return undefined;
};

export const getUserUuidFromUrl = () => {
  let urlParams = getUrlParam();
  if (urlParams.has("userId")) {
    return urlParams.get("userId");
  }

  return undefined;
};

export const getIsLoggedFromUrl = () => {
  let urlParams = getUrlParam();
  if (urlParams.has("isLogged")) {
    return urlParams.get("isLogged");
  }

  return undefined;
};
