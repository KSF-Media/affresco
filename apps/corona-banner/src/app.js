import React, { useState, useEffect } from 'react'
import axios from 'axios'
import CoronaSvg from './assets/covid-virus-icon.svg'
import Chevron from '../../../images/chevron.svg'

const Banner = ({ newCases, hospitalised, deaths, vaccinated, vaccinatedPercentage }) => {
  return (
    <div className='corona-container'>
      <div className='content-container'>
        <header className='container-header'>
          <h1 className='banner-title'>
            Covid-19 <br /> i Finland
          </h1>
          <img className='virus-image' src={CoronaSvg} alt='' />
        </header>
        {newCases !== null && 
          <div className='stat'>
          <div className='stat-value'>{newCases}</div> 
            <div className='stat-label'>nya fall</div>
          </div>
        }
        { hospitalised !== null &&
          <div className='stat mobile-hidden'>
            <div className='stat-value'>{hospitalised}</div>
            <div className='stat-label'>på sjukhus</div>
          </div>
        }
        { deaths !== null &&
        <div className='stat'>
          <div className='stat-value'>{deaths}</div>
          <div className='stat-label'>avlidna</div>
        </div>
        }
        { vaccinated !== null &&
          <div className='stat'>
            <div className='stat-value'>
              {vaccinated} <span className='stat-percent'>({vaccinatedPercentage}%)</span>
            </div>
            <div className='stat-label'>vaccinerade</div>
          </div>
        }
      </div>
      <div className='chevron-container'>
        <img className='chevron-right' src={Chevron} alt='' />
      </div>
    </div>
  )
}

export default function App() {
  const [newCases, setNewCases] = useState(null)
  const [hospitalised, setHospitalised] = useState(null)
  const [deaths, setDeaths] = useState(null)
  const [vaccinated, setVaccinated] = useState(null)
  const [vaccinatedPercentage, setVaccinatedPercentage] = useState(null)
  const [isLoaded, setIsLoaded] = useState(false)


  useEffect(() => {
    console.log('effect')
    axios
      .get(' https://cdn.ksfmedia.fi/corona-banner/stats.json')
      .then(response => {
        console.log('promise fulfilled')
        console.log(response.data)
        setNewCases(response.data.newCases)
        setHospitalised(response.data.hospitalised)
        setDeaths(response.data.deaths)
        setVaccinated(response.data.vaccinated)
        setVaccinatedPercentage(response.data.vaccinatedPercentage)
      })
      .then(() => {
        setIsLoaded(true)
      })
      .catch(error => console.log(error))
  }, [])

  

  return isLoaded ? (
    <Banner
      newCases={newCases}
      hospitalised={hospitalised}
      deaths={deaths}
      vaccinated={vaccinated}
      vaccinatedPercentage={vaccinatedPercentage}
    />
  ) : null
}
