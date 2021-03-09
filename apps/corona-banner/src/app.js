import React, { useState, useEffect } from 'react'
import axios from 'axios'
import CoronaSvg from './assets/covid-virus-icon.svg'
import Chevron from '../../../images/chevron.svg'

export default function App() {
  const [value1, setValue1] = useState(null)
  const [value2, setValue2] = useState(null)
  const [value3, setValue3] = useState(null)
  const [value4, setValue4] = useState(null)


  useEffect(() => {
    console.log('effect')
    axios.get('https://cat-fact.herokuapp.com/facts').then(response => {
      console.log('promise fulfilled')
      console.log(response)
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
          <div className='stat-value'>590</div>
          <div className='stat-label'>smittade nu</div>
        </div>
        <div className='stat mobile-hidden'>
          <div className='stat-value'>186</div>
          <div className='stat-label'>på sjukhus</div>
        </div>
        <div className='stat'>
          <div className='stat-value'>734</div>
          <div className='stat-label'>avlidna</div>
        </div>
        <div className='stat'>
          <div className='stat-value'>
            288476 <span className='stat-percent'>(5,2%)</span>
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
