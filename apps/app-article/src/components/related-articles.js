import React, {Component, Fragment} from 'react';
import hblDefaultImage from "../assets/images/hbl-fallback-img.png";
import { isDarkModeOn } from '../helper';
import Card from "./card";
import MobileList from "./moble-article-list";
import TabletList from "./tablet-article-list";

class RelatedArticles extends Component {
    constructor(props) {
        super(props);
    }

    render() {
        return (
            <div className={"relatedArticles"}>
                <div className={"row"}>
                    <div className={"col-12"}>
                        <h3 className={`latest ${isDarkModeOn() ? 'darkMode': ''}`}>Andra läser</h3>
                    </div>
                </div>
                <div className={"mobileView"}>
                    <MobileList articles = {this.props.relatedArticles}/>
                </div>

                <div className={"tabletView"}>
                    <div className={"row articleItem"}>
                        <TabletList articles = {this.props.relatedArticles}/>
                    </div>
                </div>
            </div>
        )
    }
}

export default RelatedArticles
