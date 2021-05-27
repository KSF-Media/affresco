import React, { useState, useEffect } from "react"
import axios from "axios"
import CoronaBanner from "../../../packages/components/src/CoronaBanner/banner"

function getSiteUrl() {
  const queryParameter = window.location.search
  const siteRegEx = /site=(\w+)/
  const siteArray = queryParameter.match(siteRegEx) || []

  if (siteArray.includes("on")) {
    return "https://www.ostnyland.fi/tagg/coronaviruset/"
  } else if (siteArray.includes("vn")) {
    return "https://www.vastranyland.fi/tagg/coronaviruset/"
  } else {
    return "https://www.hbl.fi/tagg/coronaviruset/"
  }
}

export default function App() {
  const [isLoaded, setIsLoaded] = useState(false)
  const [banner, setBanner] = useState(null)

  useEffect(() => {
    axios
      .get("https://cdn.ksfmedia.fi/corona-banner/stats.json")
      .then(response => {
        setBanner(
          <CoronaBanner
            newCases={response.data.newCases}
            hospitalised={response.data.hospitalised}
            deaths={response.data.deaths}
            vaccinated={response.data.vaccinatedAmount}
            vaccinatedPercentage={response.data.vaccinatedPercentage}
            siteUrl={getSiteUrl()}
            showLinks={true}
          />
        )
      })
      .then(() => {
        setIsLoaded(true)
      })
      .catch(error => console.log(error))
  }, [])
  return isLoaded ? banner : null
}
