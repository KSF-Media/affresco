import React, {Component} from 'react';
import PremiumBadge from "./badge";


const getTag = (tags) => {
    let tag = '';
    if (tags.length > 0) {
        tag = tags[0];
    }
    return tag;
};

const formatTime = (date) => {
    let publishingDate = new Date(date);
    let newPublishingDate = publishingDate.getDate() + '.' + (publishingDate.getMonth() + 1) + '.' + publishingDate.getFullYear() + ' ' + (publishingDate.getHours() < 10 ? '0' : '') + publishingDate.getHours() + ':' + (publishingDate.getMinutes() < 10 ? '0' : '') + publishingDate.getMinutes();
    let todaysDate = new Date();
    let publishedTime = (publishingDate.getHours() < 10 ? '0' : '') + publishingDate.getHours() + ':' + (publishingDate.getMinutes() < 10 ? '0' : '') + publishingDate.getMinutes();

    if (publishingDate.setHours(0, 0, 0, 0) === todaysDate.setHours(0, 0, 0, 0)) {
        newPublishingDate = publishedTime;
    }
    return newPublishingDate;
};

const Card = (props) => {
    return(
        <div className={"card"} onClick={() => { window.location.href = "?uuid=" + props.article.uuid; }}>
            <div className={"article-main-image"} >
                {
                    props.article.premium ?
                        <div className={"article-premium-badge"}><PremiumBadge/></div>
                        : ''
                }
                <img className={"card-img-top"} src={props.article.listImage.url + "&function=hardcrop&width=798&height=649&q=95"}/>
            </div>
            <div className={"card-body"}>
                <h5 className={"card-title"}>
                    <strong>
                        <a className={"relatedArticlesItem"}
                           href={"?uuid=" + props.article.uuid}>
                            {
                                props.article.title.length > 50 ?
                                    props.article.title.substring(0, 50) + "..."
                                    :
                                    props.article.title
                            }
                        </a>
                    </strong>
                </h5>

                <div className={"articleItemDetails"} style={{bottom: 0, position: 'absolute'}}>
                    <div className={"category"}>{getTag(props.article.tags)}</div>
                    <div className={"date"} style={{display: 'block'}}>{formatTime(props.article.publishingTime)}</div>
                </div>
            </div>
        </div>
    )
};

export default Card
