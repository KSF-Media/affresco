import React, { Component } from 'react';
import { getBrandValueParam } from '../helper';

const PremiumBox = (props) => {
    return (
        <div className={`ksf-prenpuff content premiumBox articleteaser ${getBrandValueParam()}`}>
            <h3>Fint att du är intresserad av artikeln!</h3>
            <br />
            <p>
                En del av vårt material är endast tillgängligt för våra
                prenumeranter.
            </p>
            <p className="cta-login-existing">Redan kund? <a
                onClick={(e) => props.showLogin(e)} href={"#"}>Logga in</a>
            </p>
        </div>
    )
};

export default PremiumBox