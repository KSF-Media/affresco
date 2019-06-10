import React, {Component} from 'react';

const Footer = (props) => {
    return(
        <div className={"footer"}>
            <a href="https://docs.google.com/forms/d/e/1FAIpQLSdu6q3J4b-FIQQfx2rGfPq0NUbWsc8hYYZP0unCHpt22thyAg/viewform?usp=sf_link">Ge
                respons</a>
            <br/><strong>KSF Media {new Date().getFullYear()}</strong>
        </div>
    )
};

export default Footer