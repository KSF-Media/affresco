import React, { Component, Fragment } from "react";
import PremiumBadge from "./badge";

const ArticleType = (props) => {
  return (
    <span className="article-opinion-type">
      {!!props.articleTypeDetails ? props.articleTypeDetails.title.toUpperCase() : ""}
    </span>
  );
};

const ArticleDetails = (props) => {
  let publishingDate = new Date(props.publishingTime);
  let updatingDate = new Date(props.updateTime);
  const isUpdateTimeEarlierThanPublishTime = publishingDate.getTime() > updatingDate.getTime();

  let newPublishingDate =
    publishingDate.getDate() +
    "." +
    (publishingDate.getMonth() + 1) +
    "." +
    publishingDate.getFullYear() +
    " " +
    (publishingDate.getHours() < 10 ? "0" : "") +
    publishingDate.getHours() +
    ":" +
    (publishingDate.getMinutes() < 10 ? "0" : "") +
    publishingDate.getMinutes();
  let newUpdateDate =
    updatingDate.getDate() +
    "." +
    (updatingDate.getMonth() + 1) +
    "." +
    updatingDate.getFullYear() +
    " " +
    (updatingDate.getHours() < 10 ? "0" : "") +
    updatingDate.getHours() +
    ":" +
    (updatingDate.getMinutes() < 10 ? "0" : "") +
    updatingDate.getMinutes();

  let todaysDate = new Date();

  let publishedTime =
    (publishingDate.getHours() < 10 ? "0" : "") +
    publishingDate.getHours() +
    ":" +
    (publishingDate.getMinutes() < 10 ? "0" : "") +
    publishingDate.getMinutes();
  let updatedTime =
    (updatingDate.getHours() < 10 ? "0" : "") +
    updatingDate.getHours() +
    ":" +
    (updatingDate.getMinutes() < 10 ? "0" : "") +
    updatingDate.getMinutes();

  if (publishingDate.setHours(0, 0, 0, 0) === todaysDate.setHours(0, 0, 0, 0)) {
    newPublishingDate = publishedTime;
  }

  if (updatingDate.setHours(0, 0, 0, 0) === todaysDate.setHours(0, 0, 0, 0)) {
    newUpdateDate = updatedTime;
  }

  if (isUpdateTimeEarlierThanPublishTime) {
    newPublishingDate = newUpdateDate;
  }

  const isCategoryOpinion = () => {
    return props.category === "Opinion";
  };

  return (
    <div className={"container"}>
      {props.category !== "Advertorial" && (
        <div className={"row articleDetails"}>
          {isCategoryOpinion() ? (
            <Fragment>
              <div className={"col-2"} style={{ padding: "0px" }}>
                {props.authors != null
                  ? props.authors.map((author, index) => {
                      return (
                        <div className={"mb-1"} key={index}>
                          <div
                            className="authorProfilePic"
                            key={index}
                            style={{
                              backgroundImage: `url(${author.image + "?width=740&height=850&function=hardcrop"})`,
                            }}
                          ></div>
                        </div>
                      );
                    })
                  : null}
              </div>
              <div className={"col-5"} style={{ paddingLeft: "0px" }}>
                {props.authors != null
                  ? props.authors.map((author, index) => {
                      return (
                        <div key={index}>
                          <div className={"author"}> {author.byline} </div>
                        </div>
                      );
                    })
                  : null}
                {props.premium ? (
                  <div className="article-info">
                    <ArticleType articleTypeDetails={props.articleTypeDetails} /> <PremiumBadge paper={props.paper} />
                  </div>
                ) : null}
              </div>
            </Fragment>
          ) : null}

          {!isCategoryOpinion() ? (
            <div className={"col-7"} style={{ paddingLeft: "0px" }}>
              {props.authors != null ? (
                <div>
                  {props.authors.map((author, index) => {
                    return (
                      <div key={index}>
                        <div className={"author"}> {author.byline} </div>
                      </div>
                    );
                  })}
                </div>
              ) : null}
              {props.premium ? (
                <div className="article-info">
                  <ArticleType articleTypeDetails={props.articleTypeDetails} /> <PremiumBadge paper={props.paper} />
                </div>
              ) : null}
            </div>
          ) : null}

          <div className="col-5 pubDate text-right" style={{ paddingLeft: "0px", paddingRight: "0px" }}>
            <div>Pub. {newPublishingDate}</div>
            {!isUpdateTimeEarlierThanPublishTime && <div style={{ marginTop: "-3px" }}>Uppd. {newUpdateDate}</div>}
          </div>
        </div>
      )}
    </div>
  );
};

export default ArticleDetails;
