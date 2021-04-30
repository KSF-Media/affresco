import config from "./config";
import { isUserLoggedIn, getBrandValueParam, getUrlParam, getTokenFromUrl, getUserUuidFromUrl } from "./helper";
import Cookies from 'js-cookie';

const articleApi = {
    getArticle(uuid) {
        return fetch(config.apiUrl + "/article/" + uuid, {
            method: 'GET',
            headers: attachHeaders()
        })
            .then(response => response.json())

    },
    getLatestArticles() {
        return fetch(config.apiUrl + "/latest?start=0&limit=10&paper=" + getBrandValueParam(), {
            method: 'GET',
        })
            .then(response => response.json())
    },
    getMostReadArticles() {
        const brand = getBrandValueParam();
        //We only want exclusively subscriber data for hbl, others can have all data
        return fetch(`${config.apiUrl}/mostread?start=0&limit=10&paper=${brand}&onlySubscribers=${brand === 'hbl'}`, {
            method: 'GET',
        })
            .then(response => response.json())
    }
};

function attachHeaders() {
    let headers = { 'Content-Type': 'application/json' };

    const uuid = localStorage.getItem('uuid') || Cookies.get('uuid') || getUserUuidFromUrl();
    const token = localStorage.getItem('token') || Cookies.get('token') || getTokenFromUrl();

    if (isUserLoggedIn()) {
        headers = {
            'Content-Type': 'application/json',
            'AuthUser': uuid,
            'Authorization': 'OAuth ' + token,
        };
    }
    return headers;
}

export default articleApi