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
  try {
    Android.showLogin();
  } catch (err) {
    // we might have a crash when Android bridge is not registered
  }

  // For Ios
  try {
    Window.webkit.messageHandlers.showLogin.postMessage("");
  } catch (err) {
    // we might have a crash when Ios webkit bridge is not registered
  }
}

export default PremiumBox;
