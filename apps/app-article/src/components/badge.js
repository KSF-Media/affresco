import React from 'react';
import {getBrandValueParam} from "../helper";

const PremiumBadge = () => {
    return(
        <span className={`premiumBadge brandBg-${getBrandValueParam()}`}>PREMIUM</span>
    )
};

export default PremiumBadge