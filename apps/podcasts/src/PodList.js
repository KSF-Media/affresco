// src/PodList.js
import React from "react";

import TrackList from "./Tracklist.js"

class PodList extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      loaded: false,
      userIds: this.props.userIds || [],
      podcasts: []
    }
  }

  componentDidMount() {
    const promises = this.state.userIds.map(id => 
      fetch(`https://api.soundcloud.com/users/${id}/?client_id=${process.env.CLIENT_ID}`)
      .then(res => res.json())
    );
    Promise.all(promises).then(
      json => {
        this.setState({podcasts: [...json]});
        // console.log("Pods: ", this.state.podcasts);
      }
    )
  }

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

export default PodList;