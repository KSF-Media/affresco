import React, {Component} from 'react';
import hblDefaultImage from "../assets/images/hbl-fallback-img.png";

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
        let latestArticles = [];
        if (this.isArray(this.props.manuallyRelatedArticles)) {
            latestArticles = this.props.manuallyRelatedArticles.map((item, index) => {
                return (
                    <div key={index} className={"col-12 articleItem manuallyRelated"}>
                        <div className={"row"} style={{paddingLeft: '8px'}}>
                            <div className={"col-8 my-auto"} onClick={() => {
                                window.location.href = "?uuid=" + item.uuid;
                            }} style={{paddingLeft: '0px'}}>
                                <div>
                                    <a className={"relatedArticlesItem"}
                                       href={"?uuid=" + item.uuid}>
                                        {
                                            item.title.length > 80 ?
                                                item.title.substring(0, 80) + "..."
                                                :
                                                item.title
                                        }
                                    </a>
                                </div>
                                <div className={"articleItemDetails"}>
                                    <div className={"category"}>{this.getTag(item.tags)}</div>
                                    <div className={"date"}>{this.formatTime(item.publishingTime)}</div>
                                </div>
                            </div>
                            <div className={"col-4 articleImage"} onClick={() => {
                                window.location.href = "?uuid=" + item.uuid;
                            }} style={{padding: '0px'}}>
                                {
                                    item.listImage === null ?
                                        <img className="card-img-top"
                                             src={hblDefaultImage}
                                              alt=""/>
                                        :
                                        <img className="card-img-top"
                                             src={item.listImage.url + "&width=120&height=100&function=hardcrop"}
                                              alt=""/>
                                }
                            </div>
                        </div>
                    </div>
                )
            });
        }
        return (
            <div className={"relatedArticles"}>
                <div className={"row"}>
                    <div className={"col-12"}>
                        <h3 className={"latest"}>Läs ocksa:</h3>
                    </div>
                </div>
                {latestArticles}
                <div className={"col-12 bottomLine"}></div>
            </div>
        )
    }
}

export default ManuallyRelatedArticles