import React from "react";

const DepricationWarning = () => {
    const warningstyle = {
        border: "10px solid red",
        background: "white",
        padding: "10px",
        fontSize: "1.3rem",
        lineHeight: "120%",
        fontWeight: "bolder"

    }

    return (
        <div className={"col-12 mt-2 mb-3"} style={warningstyle}>
            Bästa läsare! Den här versionen av appen kommer snart att sluta fungera. 
            Du måste uppdatera om du vill fortsätta använda appen.
        </div>
    )
}

export default DepricationWarning;