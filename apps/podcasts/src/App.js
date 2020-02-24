import React from "react";
import {Link, Route} from "react-router-dom";

import PodList from "./PodList.js";
import LatestTracksList from "./LatestTracsklist.js";

class App extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      loaded: false,
      userIds: this.props.userIds || [],
      users: [],
      tracks: []
    }
  }

  componentDidMount() {
    const users = this.state.userIds.map(id => 
      fetch(`https://api.soundcloud.com/users/${id}/?client_id=${process.env.CLIENT_ID}`)
      .then(res => res.json())
    );
    Promise.all(users).then(
      json => {
        this.setState({users: [...json]});
      }
    )

    const tracks = this.state.userIds.map(id => 
      fetch(`https://api.soundcloud.com/users/${id}/tracks/?client_id=${process.env.CLIENT_ID}`)
      .then(res => res.json())
    );
    Promise.all(tracks).then(
      json => {
        this.setState({tracks: this.state.tracks.concat(...json)});
      }
    )
  }
  
  render() {

    const sortedTracks = this.state.tracks.sort((a,b) => {
      const x = new Date(a.created_at).getTime();
      const y = new Date(b.created_at).getTime();
      return y - x;
    })

    return (
      <div>
        <Route path="/" render={ (props) => <LatestTracksList tracks={sortedTracks} />} />
      </div>
    )
  }

}

export default App;