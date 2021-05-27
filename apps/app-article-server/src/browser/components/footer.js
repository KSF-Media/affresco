import React, {Component} from 'react';

const getBrandMessage = (brand) => {
    if (brand === 'hbl') {
        return 'En obundet liberalt borgerlig tidning';
    }
};

const Footer = (props) => {
    return(
        <div className={"footer"}>
            <h4 className={"headline"}>{getBrandMessage(props.brandValueName)}</h4>
            <a href="https://docs.google.com/forms/d/e/1FAIpQLSdORLcRq_URIS7deTkgOKfGd1fy59lkdTEtEbt5bzvaADCDKw/viewform?usp=sf_link">Ge
                respons</a>
            <br/><strong>KSF Media {new Date().getFullYear()}</strong>
        </div>
    )
};

export default Footer
