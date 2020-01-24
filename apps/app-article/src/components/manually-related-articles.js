import React, {Component} from 'react';
import hblDefaultImage from "../assets/images/hbl-fallback-img.png";
import MobileList from "./moble-article-list";
import TabletList from "./tablet-article-list";

class ManuallyRelatedArticles extends Component {
    constructor(props) {
        super(props);
    }

    isArray(value) {
        return value && typeof value === 'object' && value.constructor === Array;
    }

    formatTime(date){
        let publishingDate = new Date(date);
        let newPublishingDate = publishingDate.getDate() + '.' + (publishingDate.getMonth() + 1) + '.' + publishingDate.getFullYear() + ' ' + (publishingDate.getHours() < 10 ? '0' : '') + publishingDate.getHours() + ':' + (publishingDate.getMinutes() < 10 ? '0' : '') + publishingDate.getMinutes();
        let todaysDate = new Date();
        let publishedTime = (publishingDate.getHours() < 10 ? '0' : '') + publishingDate.getHours() + ':' + (publishingDate.getMinutes() < 10 ? '0' : '') + publishingDate.getMinutes();

        if (publishingDate.setHours(0, 0, 0, 0) === todaysDate.setHours(0, 0, 0, 0)) {
            newPublishingDate = publishedTime;
        }
        return newPublishingDate;
    }

    getTag(tags){
        let tag = '';
        if (tags.length > 0) {
            tag = tags[0];
        }
        return tag;
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
