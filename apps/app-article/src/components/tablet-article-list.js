import React, {Component} from 'react';
import Card from "./card";


const isArray = (value) => {
    return value && typeof value === 'object' && value.constructor === Array;
};

const TabletList = (props) => {
    let relatedArticles = [];
    if (isArray(props.articles)) {
        relatedArticles = props.articles.map((item, index) => {
            return (
                <div className={"col-4 articleItem mt-n1"} key={index}>
                    <Card article = {item} />
                </div>
            )
        });
        return relatedArticles;
    }
};

export default TabletList
