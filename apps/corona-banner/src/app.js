import React, { useState, useEffect } from 'react'
import axios from 'axios'
import CoronaBanner from '../../../packages/components/src/CoronaBanner/banner'

export default function App() {
  const [isLoaded, setIsLoaded] = useState(false)
  const [banner, setBanner] = useState(null)

  useEffect(() => {
    axios
      .get('https://cdn.ksfmedia.fi/corona-banner/stats.json')
      .then(response => {
        setBanner(
          <CoronaBanner
            newCases={response.data.newCases}
            hospitalised={response.data.hospitalised}
            deaths={response.data.deaths}
            vaccinated={response.data.vaccinated}
            vaccinatedPercentage={response.data.vaccinatedPercentage}
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
