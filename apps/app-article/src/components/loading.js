import React, {Component} from 'react';
import loadingIcon from "../assets/images/loading.gif";

const Loading = () => {
    return(
        <div className="loading">
            <img src={loadingIcon}/>
        </div>
    )
};

export default Loading
