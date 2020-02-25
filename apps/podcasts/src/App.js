import React from "react";
import {Link, Route} from "react-router-dom";

import LatestTracksList from "./LatestTracsklist.js";

class App extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      loaded: false,
      userIds: this.props.userIds || [],
      users: [],
      tracks: [],
      selectedUser: null
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

  selectUser = (id) => {
    this.setState({
      selectedUser: id
    })
  }

  render() {

    const usersList = this.state.users.map((u, key) => {
      let buttonClass = this.state.selectedUser === u.id ? 'active' : '';
      return (<li className={buttonClass} onClick={() => this.selectUser(u.id)} key={u.id}>{u.username}</li>)
    });
    const resetList = this.state.selectedUser
      ? <li onClick={() => {this.setState({selectedUser: null})}}>Se alla</li>
      : null;

    const filteredTracks = this.state.selectedUser
      ? this.state.tracks.filter(t => t.user_id === this.state.selectedUser)
      : this.state.tracks;

    const sortedTracks = filteredTracks.sort((a,b) => {
      const x = new Date(a.created_at).getTime();
      const y = new Date(b.created_at).getTime();
      return y - x;
    })

    return (
      <div className="pod-app">
        <ul className="pod-nav">
          <li className="label">Filtrera podcasts</li>
          {usersList}
          {resetList}
        </ul>
        <Route path="/" render={ (props) => <LatestTracksList tracks={sortedTracks} />} />
      </div>
    )
  }

}

export default App;