import React, { useState, useEffect } from 'react'
import axios from 'axios'
import CoronaSvg from './assets/covid-virus-icon.svg'
import Chevron from '../../../images/chevron.svg'

export default function App() {
  const [newCases, setNewCases] = useState(590)
  const [hospitalised, setHospitalised] = useState(null)
  const [deaths, setDeaths] = useState(null)
  const [vaccinated, setVaccinated] = useState(288476)
  const [vaccinatedPercentage, setVaccinatedPercentage] = useState(5.2)


  useEffect(() => {
    console.log('effect')
    axios.get('https://w3qa5ydb4l.execute-api.eu-west-1.amazonaws.com/prod/finnishCoronaHospitalData').then(response => {
      console.log('promise fulfilled')
      console.log(response.data.hospitalised[1330])
      setHospitalised(response.data.hospitalised[1330].totalHospitalised)
      setDeaths(response.data.hospitalised[1330].dead)
    })
  }, [])

  return (
    <div className='corona-container'>
      <div className='content-container'>
        <header className='container-header'>
          <h1 className='banner-title'>
            Covid-19 <br /> i Finland
          </h1>
          <img className='virus-image' src={CoronaSvg} alt='Coronavirus cell' />
        </header>
        <div className='stat'>
          <div className='stat-value'>{newCases}</div>
          <div className='stat-label'>nya fall</div>
        </div>
        <div className='stat mobile-hidden'>
          <div className='stat-value'>{hospitalised}</div>
          <div className='stat-label'>på sjukhus</div>
        </div>
        <div className='stat'>
          <div className='stat-value'>{deaths}</div>
          <div className='stat-label'>avlidna</div>
        </div>
        <div className='stat'>
          <div className='stat-value'>
            {vaccinated} <span className='stat-percent'>({vaccinatedPercentage}%)</span>
          </div>
          <div className='stat-label'>vaccinerade</div>
        </div>
      </div>
      <div className='chevron-container'>
        <img className='chevron-right' src={Chevron} alt='' />
      </div>
    </div>
  )
}
