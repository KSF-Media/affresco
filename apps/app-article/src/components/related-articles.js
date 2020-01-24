import React, {Component, Fragment} from 'react';
import hblDefaultImage from "../assets/images/hbl-fallback-img.png";
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
                        <h3 className={"latest"}>Mest lästa</h3>
                    </div>
                </div>
                <div className={"mobileView"}>
                    <div className={"row articleItem"}>
                        <MobileList articles = {this.props.relatedArticles}/>
                    </div>
                </div>

                <div className={"tabletView"}>
                    <div className={"row articleItem"}>
                        <TabletList articles = {this.props.relatedArticles}/>
                    </div>
                </div>
                <div className={"col-12 bottomLine"}></div>
            </div>
        )
    }
}

export default RelatedArticles