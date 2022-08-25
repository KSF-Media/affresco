import { TCString } from '@iabtcf/core';
import Cookies from 'js-cookie'

function checkEncodedConsentCookie(id) {
    console.log("checkEncodedConsentCookie")
      if (Cookies.get('FCCDCF') == undefined) {
          return
      } else {
          setDecodedConsentCookie(Cookies.get('FCCDCF'))
          window.clearInterval(id)
      }
  }
  
  function setDecodedConsentCookie(encodedCookie) {
      let consents = {}
      const decodedTCString = TCString.decode(JSON.parse(encodedCookie)[3][0]);
      decodedTCString.purposeConsents.has(1) ? consents["store_cookies"]="granted" : consents["store_cookies"]="denied"
      Cookies.set("consents", JSON.stringify(consents))
      console.log("set consent cookie to: ", consents)
  }
  
  export function startConsentCookieSetup() {
      let consents = {"store_cookies": "denied"}
      console.log("initial consent cookie: ", consents)
      Cookies.set("consents", JSON.stringify(consents))
      if (Cookies.get('FCCDCF') == undefined) {
          const id = window.setInterval(function () { checkEncodedConsentCookie(id) }, 1000)
      } else {
          setDecodedConsentCookie(Cookies.get('FCCDCF'))
      }
  }