import config from "./config";
import { isUserLoggedIn, getBrandValueParam } from "./helper";

const UUID = require('uuid');

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

    const uuid = localStorage.getItem('uuid');
    const token = localStorage.getItem('token');

    if (isUserLoggedIn() && uuid && UUID.validate(uuid) && token) {
        headers = {
            'Content-Type': 'application/json',
            'AuthUser': localStorage.getItem('uuid'),
            'Authorization': 'OAuth ' + localStorage.getItem('token'),
        };
    }
    return headers;
}

export default articleApi
