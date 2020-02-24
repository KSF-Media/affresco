// src/PodList.js
import React from "react";

import LatestTrackTrackList from "./LatestTracsklist.js"

class PodTracksList extends React.Component {

  render() {
    const listItems = this.state.podcasts.map((p, k) => {
      return (
        <div key={k} className="pod">
          <h1>{p.username}</h1>
          <div
            className="pod-description">
            <img
              className="pod-avatar"
              src={p.avatar_url}
            />
            <div>
              {p.description}
            </div>
          </div>
          <TrackList 
            podId={p.id}
          />
        </div>
      )
    });

    return (
      <div>
        {listItems}
      </div>
    )
  }
}

export default PodTracksList;