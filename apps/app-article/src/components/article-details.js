import React, {Component, Fragment} from 'react';
import PremiumBadge from "./badge";

const ArticleDetails = (props) => {

    let publishingDate = new Date(props.publishingTime);
    let updatingDate = new Date(props.updateTime);

    let newPublishingDate = publishingDate.getDate() + '.' + (publishingDate.getMonth() + 1) + '.' + publishingDate.getFullYear() + ' ' + (publishingDate.getHours() < 10 ? '0' : '') + publishingDate.getHours() + ':' + (publishingDate.getMinutes() < 10 ? '0' : '') + publishingDate.getMinutes();
    let newUpdateDate = updatingDate.getDate() + '.' + (updatingDate.getMonth() + 1) + '.' + updatingDate.getFullYear() + ' ' + (updatingDate.getHours() < 10 ? '0' : '') + updatingDate.getHours() + ':' + (updatingDate.getMinutes() < 10 ? '0' : '') + updatingDate.getMinutes();

    let todaysDate = new Date();

    let publishedTime = (publishingDate.getHours() < 10 ? '0' : '') + publishingDate.getHours() + ':' + (publishingDate.getMinutes() < 10 ? '0' : '') + publishingDate.getMinutes();
    let updatedTime = (updatingDate.getHours() < 10 ? '0' : '') + updatingDate.getHours() + ':' + (updatingDate.getMinutes() < 10 ? '0' : '') + updatingDate.getMinutes();

    if (publishingDate.setHours(0, 0, 0, 0) === todaysDate.setHours(0, 0, 0, 0)) {
        newPublishingDate = publishedTime;
    }

    if (updatingDate.setHours(0, 0, 0, 0) === todaysDate.setHours(0, 0, 0, 0)) {
        newUpdateDate = updatedTime;
    }

    const isCategoryOpinion = () => {
        return props.category === 'Opinion';
    };

    return (
        <div className={"container"}>
            <div className={"row articleDetails"}>
                {
                    isCategoryOpinion() ?
                        <Fragment>
                            <div className={"col-2"} style={{padding: '0px'}}>
                                {
                                    props.authors != null ?
                                        props.authors.map((author, index) => {
                                            return (
                                                <div className={"mb-1"} key={index}>
                                                    <img  style={{borderRadius: '50%'}} width={"38px"}
                                                         height={"38px"}
                                                         src={author.image}
                                                         alt=""/>
                                                </div>
                                            )
                                        })
                                        :
                                        ''
                                }
                                {
                                    props.premium ?
                                        <PremiumBadge/>
                                        : ''
                                }
                            </div>
                            <div className={'col-5'} style={{paddingLeft: "0px"}}>
                                {
                                    props.authors != null ?
                                        props.authors.map((author, index) => {
                                            return (
                                                <div className={"mb-2"} key={index}>
                                                    <div className={"author"}> {author.byline} </div>
                                                </div>
                                            )
                                        })
                                        :
                                        ''
                                }
                            </div>
                        </Fragment>
                        :
                        ''
                }

                {
                    !isCategoryOpinion() ?
                        <div className={'col-7'} style={{paddingLeft: "0px"}}>
                            {
                                props.authors != null ?
                                    <div>
                                        {
                                            props.authors.map((author, index) => {
                                                return (
                                                    <div key={index}>
                                                        <div className={"author"}> {author.byline} </div>
                                                    </div>
                                                )
                                            })
                                        }
                                    </div>
                                    : ''
                            }
                            {
                                props.premium ?
                                    <PremiumBadge/>
                                    : ''
                            }
                        </div>
                        :
                        ''
                }


                <div className="col-5 pubDate text-right"
                     style={{paddingLeft: "0px", paddingRight: "0px"}}>
                    <div>Pub. {newPublishingDate}</div>
                    <div style={{marginTop: "-3px"}}>Uppd. {newUpdateDate}</div>
                </div>
            </div>
        </div>
    )
};

export default ArticleDetails
