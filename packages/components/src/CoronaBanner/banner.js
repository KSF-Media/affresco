import React from 'react'
import CoronaSvg from '../../../../images/covid-virus-icon.svg'
import Chevron from '../../../../images/chevron.svg'

const CoronaBanner = ({
  newCases,
  hospitalised,
  deaths,
  vaccinated,
  vaccinatedPercentage,
}) => {
  
  function getSiteUrl() {
    const queryParameter = window.location.search
    const siteRegEx = /site=(\w+)/
    const siteArray = queryParameter.match(siteRegEx) || []

    if (siteArray.includes('on')) {
      return 'https://www.ostnyland.fi/tagg/coronaviruset/'
    } else if (siteArray.includes('vn')) {
      return 'https://www.vastranyland.fi/tagg/coronaviruset/'
    } else {
      return 'https://www.hbl.fi/tagg/coronaviruset/'
    }
  }

  return (
    <div className='container-wrapper'>
      <a href={getSiteUrl()} target='_parent'>
        <div className='corona-container'>
          <div className='content-container'>
            <header className='container-header'>
              <h1 className='banner-title'>
                Covid-19 <br /> i Finland
              </h1>
              <img className='virus-image' src={CoronaSvg} alt='' />
            </header>
            {newCases !== null && (
              <div className='stat'>
                <div className='stat-value'>{newCases}</div>
                <div className='stat-label'>nya fall</div>
              </div>
            )}
            {hospitalised !== null && (
              <div className='stat mobile-hidden'>
                <div className='stat-value'>{hospitalised}</div>
                <div className='stat-label'>på sjukhus</div>
              </div>
            )}
            {deaths !== null && (
              <div className='stat'>
                <div className='stat-value'>{deaths}</div>
                <div className='stat-label'>dödsfall</div>
              </div>
            )}
            {vaccinated !== null && (
              <div className='stat'>
                <div className='stat-value'>
                  {vaccinated}{' '}
                  <span className='stat-percent'>
                    ({vaccinatedPercentage}%)
                  </span>
                </div>
                <div className='stat-label'>vaccinerade</div>
              </div>
            )}
          </div>
          <div className='chevron-container'>
            <img className='chevron-right' src={Chevron} alt='' />
          </div>
        </div>
      </a>
      <div className='source'>
        <em>
          Källa:{' '}
          <a
            className='corona-banner-source-link'
            target='_blank'
            href='https://thl.fi/sv/web/thlfi-sv/statistik-och-data/material-och-tjanster/oppna-data'
          >
            THL
          </a>{' '}
          och{' '}
          <a
            className='corona-banner-source-link'
            target='_blank'
            href='https://github.com/HS-Datadesk/koronavirus-avoindata'
          >
            HS Open Data
          </a>
        </em>
      </div>
    </div>
  )
}

export default CoronaBanner;
