import React, { Component } from "react";

const PremiumBox = (props) => {
  return (
    <div className={`ksf-prenpuff content premiumBox articleteaser ${props.paper}`}>
      <h3>Fint att du är intresserad av artikeln!</h3>
      <br />
      <p>En del av vårt material är endast tillgängligt för våra prenumeranter.</p>
      <p className="cta-login-existing">
        Redan kund?{" "}
        <a onClick={(e) => showLogin(e)} href={"#"}>
          Logga in
        </a>
      </p>
    </div>
  );
};

function showLogin(e) {
  e.preventDefault();
  // TODO: Add mobile bridge
  console.log("SHOW LOGIN");
}

export default PremiumBox;
