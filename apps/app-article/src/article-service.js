import config from "./config";
import {getCookie, isUserLoggedIn} from "./helper";

const articleApi = {
    getArticle(uuid) {
        return fetch(config.apiUrl + "article/" + uuid, {
            method: 'GET',
            headers: attachHeaders()
        })
            .then(response => response.json())

    },
    getLatestArticles() {
        return fetch(config.apiUrl + "latest?start=0&limit=5", {
            method: 'GET',
        })
            .then(response => response.json())
    },
    getMostReadArticles() {
        return fetch(config.apiUrl + "mostread?start=0&limit=5", {
            method: 'GET',
        })
            .then(response => response.json())
    }
};

function attachHeaders() {
    let headers = {'Content-Type': 'application/json'};
    if (isUserLoggedIn()) {
        headers = {
            'Content-Type': 'application/json',
            'AuthUser': localStorage.getItem('uuid'),
            'Authorization': 'OAuth ' + getCookie('token'),
        };
    }
    return headers;
}

export default articleApi