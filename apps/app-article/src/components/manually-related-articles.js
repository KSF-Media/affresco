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
            <div className={"relatedArticles "}>
                <div className={"row"}>
                    <div className={"col-12"}>
                        <h3 className={"latest"}>Läs också</h3>
                    </div>
                </div>
                <MobileList articles = {this.props.manuallyRelatedArticles}/>
            </div>
        )
    }
}

export default ManuallyRelatedArticles
