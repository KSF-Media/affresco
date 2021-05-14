import React, { Fragment } from "react"
import CoronaSvg from "../../../../images/covid-virus-icon.svg"
import Chevron from "../../../../images/chevron.svg"
import "../../../../less/corona-banner.less"

const LinkWrapper = ({ condition, wrapper, children }) =>
  condition ? wrapper(children) : children

const CoronaBanner = ({
  newCases,
  hospitalised,
  deaths,
  vaccinated,
  vaccinatedPercentage,
  siteUrl,
  showLinks,
}) => {
  const showVaccinated = () => {
    if (vaccinated !== null) {
      return (
        <div className="stat-value">
          {vaccinated}{" "}
          <span className="stat-percent">({vaccinatedPercentage}%)</span>
        </div>
      )
    } else {
      return (
        <div className="stat-value">
          <span className="stat-percent">{vaccinatedPercentage}%</span>
        </div>
      )
    }
  }

  return (
    <div className="corona-banner-wrapper">
      <LinkWrapper
        condition={showLinks}
        wrapper={children => (
          <a href={siteUrl} target="_parent">
            {children}
          </a>
        )}
      >
        <Fragment>
          <div className="corona-container">
            <div className="content-container">
              <header className="container-header">
                <h1 className="banner-title">
                  Covid-19 <br /> i Finland
                </h1>
                <img className="virus-image" src={CoronaSvg} alt="" />
              </header>
              {newCases !== null && (
                <div className="stat">
                  <div className="stat-value">{newCases}</div>
                  <div className="stat-label">nya fall</div>
                </div>
              )}
              {hospitalised !== null && (
                <div className="stat mobile-hidden">
                  <div className="stat-value">{hospitalised}</div>
                  <div className="stat-label">på sjukhus</div>
                </div>
              )}
              {deaths !== null && (
                <div className="stat smallest-screens-hidden">
                  <div className="stat-value">{deaths}</div>
                  <div className="stat-label">dödsfall</div>
                </div>
              )}
              {(vaccinated !== null || vaccinatedPercentage !== null) && (
                <div className="stat">
                  {showVaccinated()}
                  <div className="stat-label">vaccinerade</div>
                </div>
              )}
            </div>
            {showLinks && (
              <div className="chevron-container">
                <img className="chevron-right" src={Chevron} alt="" />
              </div>
            )}
          </div>
        </Fragment>
      </LinkWrapper>
      <div className="source">
        <em>
          Källa:{" "}
          <LinkWrapper
            condition={showLinks}
            wrapper={children => (
              <a
                className="corona-banner-source-link"
                href="https://thl.fi/sv/web/thlfi-sv/statistik-och-data/material-och-tjanster/oppna-data"
                target="_blank"
              >
                {children}
              </a>
            )}
          >
            <Fragment>THL</Fragment>
          </LinkWrapper>
          och{" "}
          <LinkWrapper
            condition={showLinks}
            wrapper={children => (
              <a
                className="corona-banner-source-link"
                href="https://github.com/HS-Datadesk/koronavirus-avoindata"
                target="_blank"
              >
                {children}
              </a>
            )}
          >
            <Fragment>HS Open Data</Fragment>
          </LinkWrapper>
        </em>
      </div>
    </div>
  )
}

export default CoronaBanner

/*
{vaccinated !== null && vaccinatedPercentage && (
                <div className="stat">
                  <div className="stat-value">
                    {vaccinated}{" "}
                    <span className="stat-percent">
                      ({vaccinatedPercentage}%)
                    </span>
                  </div>
                  <div className="stat-label">vaccinerade</div>
                </div>
              )}
              {vaccinated !== null && !vaccinatedPercentage && (
                <div className="stat">
                  <div className="stat-value">{vaccinated}</div>
                  <div className="stat-label">vaccinerade</div>
                </div>
              )}
*/
