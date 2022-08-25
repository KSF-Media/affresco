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
      console.log("setDecodedConsentCookie")
      let consents = {}
      const decodedTCString = TCString.decode(JSON.parse(encodedTCString)[3][0]);
      decodedTCString.publisherLegitimateInterests.has(8) ? consents["send_analytics"]="granted" : consents["send_analytics"]="denied"
      decodedTCString.purposeConsents.has(1) ? consents["store_cookies"]="granted" : consents["store_cookies"]="denied"
      Cookies.set("consents", JSON.stringify(consents))
  }
  
  export function startConsentCookieSetup() {
      console.log("startConsentCookieSetup")
      let consents = {"analytics_storage": "denied"}
      Cookies.set("consents", JSON.stringify(consents))
      if (Cookies.get('FCCDCF') == undefined) {
          const id = window.setInterval(function () { checkEncodedConsentCookie(id) }, 1000)
      } else {
          setDecodedConsentCookie(Cookies.get('FCCDCF'))
      }
  }

//   export function startConsentCookieSetupp() {
//     console.log("startConsentCookieSetup")
//     let consents = {"analytics_storage": "denied"}
//     Cookies.set("consents", JSON.stringify(consents))
//     if (Cookies.get('FCCDCF') == undefined) {
//         const id = window.setInterval(function () { checkEncodedConsentCookie(id) }, 1000)
//     } else {
//         setDecodedConsentCookie(Cookies.get('FCCDCF'))
//     }
// }