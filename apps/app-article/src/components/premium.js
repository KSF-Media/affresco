import React, {Component} from 'react';

const PremiumBox = (props) => {
    return(
        <div className="ksf-prenpuff content premiumBox articleteaser">
            <h3>Fint att du är intresserad av artikeln!</h3>
            <br/>
            <p className="cta_body">
                Som prenumerant får du <b>obegränsad tillgång till
                kvalitetsjournalistik</b> för endast <b>6.90 €/mån</b>.
            </p>
            <p>
                En del av vårt material är endast tillgängligt för våra
                prenumeranter (som gör det möjligt för oss att existera).<br/>Läs
                mer
                om varför i <a
                href="https://www.hbl.fi/artikel/for-knappt-sju-euro-far-du-alla-artiklar-pa-hbl-fi/">vår
                intervju med chefredaktör Susanna Landor</a>. För 6.90 €/månad
                får du tillgång till alla artiklar på hbl.fi. Du kan avsluta när du
                vill.
            </p>
            <p><a id="prenumerera-redirect--login"
                  href={`https://prenumerera.ksfmedia.fi?redirect_to=${window.location.href}`}
                  className="btn btn-cta">Köp nu!</a>
            </p>
            <p className="cta-login-existing">Redan kund? <a
                onClick={(e) => props.showLogin(e)} href={"#"}>Logga in</a>
            </p>
            <p>
                Vad är Premium? <a href="/fragor-och-svar/">Frågor och svar</a>
            </p>
        </div>
    )
};

export default PremiumBox