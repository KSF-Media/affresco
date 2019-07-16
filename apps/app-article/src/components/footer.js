import React, {Component} from 'react';

const Footer = (props) => {
    return(
        <div className={"footer"}>
            <a href="https://docs.google.com/forms/d/e/1FAIpQLSdORLcRq_URIS7deTkgOKfGd1fy59lkdTEtEbt5bzvaADCDKw/viewform?usp=sf_link">Ge
                respons</a>
            <br/><strong>KSF Media {new Date().getFullYear()}</strong>
        </div>
    )
};

export default Footer
