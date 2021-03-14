import React, {Component} from 'react';

const Header = (props) => {
    return(
        <div className={"row"}>
            <div>
                {props.mainImage != null ?
                    <img className={"header headerImage"}
                         onClick={() => props.showHighResolutionImg(props.mainImage.url, props.caption + " " + props.appendBylineLabel + " " + props.byline)}
                         src={props.mainImage.url}
                         alt=""/>
                    :
                    ''
                }
            </div>
            <div className={"col-12"}>
                <p className={"caption"} dangerouslySetInnerHTML={{__html: props.caption + " " + props.appendBylineLabel + " " + props.byline}}/>
            </div>
        </div>
    )
};

export default Header