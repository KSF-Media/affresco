import React, {Component} from 'react';
import hblDefaultImage from "../assets/images/hbl-fallback-img.png";
import MobileList from "./moble-article-list";
import TabletList from "./tablet-article-list";

class ManuallyRelatedArticles extends Component {
    constructor(props) {
        super(props);
    }
    
    render() {
        return (
            <div className={"relatedArticles"}>
                <div className={"row"}>
                    <div className={"col-12"}>
                        <h3 className={"latest"}>Läs ocksa</h3>
                    </div>
                </div>
                <div className={"mobileView"}>
                    <MobileList articles = {this.props.manuallyRelatedArticles}/>
                </div>

                <div className={"tabletView"}>
                    <div className={"row articleItem"}>
                        <TabletList articles = {this.props.manuallyRelatedArticles}/>
                    </div>
                </div>
            </div>
        )
    }
}

export default ManuallyRelatedArticles
