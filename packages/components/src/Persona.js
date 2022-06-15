"use strict";
import Persona from "persona";

Persona.ApiClient.instance.basePath = process.env.PERSONA_URL;

// https://visionmedia.github.io/superagent/#timeouts
Persona.ApiClient.instance.timeout = {
  response: 60000, // the server has a whopping 60 seconds to respond
  deadline: 20000, // but up to 20 seconds of overall data transfer
};

export const accountApi = new Persona.AccountApi(Persona.ApiClient.instance);
export const adminApi = new Persona.AdminApi(Persona.ApiClient.instance);
export const loginApi = new Persona.LoginApi(Persona.ApiClient.instance);
export const usersApi = new Persona.UsersApi(Persona.ApiClient.instance);

export function rawJSONStringify(x) {
    return JSON.stringify(x);
};

export function rawJSONParse(x) {
    const reISO = /^(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2}):(\d{2}(?:\.\d*))(?:Z|(\+|-)([\d|:]*))?$/;
    return JSON.parse(x, function (key, value) {
        if (typeof value === 'string') {
            var a = reISO.exec(value);
            if (a)
                return new Date(value);
        }
        return value;
    });
};
