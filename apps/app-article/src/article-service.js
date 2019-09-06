import config from "./config";
import {isUserLoggedIn} from "./helper";

const articleApi = {
    getArticle(uuid) {
        return fetch(config.production.apiUrl + "article/" + uuid, {
            method: 'GET',
            headers: attachHeaders()
        })
            .then(response => response.json())

    },
    getLatestArticles() {
        return fetch(config.production.apiUrl + "latest?start=0&limit=5", {
            method: 'GET',
        })
            .then(response => response.json())
    },
    getMostReadArticles() {
        return fetch(config.production.apiUrl + "mostread?start=0&limit=5", {
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
            'Authorization': 'OAuth ' + localStorage.getItem('token'),
        };
    }
    return headers;
}

export default articleApi