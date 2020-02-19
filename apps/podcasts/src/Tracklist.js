import React from 'react';

import Track from './Track.js'

class TrackList extends React.Component {
  constructor(props){
    super(props);
    this.state = {
      tracks: [],
      expanded: false
    }
    this.toggleTracks = this.toggleTracks.bind(this);
  }

  componentDidMount() {
    const podId = this.props.podId;
    fetch(`https://api.soundcloud.com/users/${podId}/tracks/?client_id=c58TXg96mhC1ETLDBCdIhbGdzSHdzqXN`)
    .then(res => res.json())
    .then(jsonData => {
      console.log("Tracks: ", jsonData);
      this.setState({
        tracks: jsonData
      });
    })
  }

  toggleTracks() {
    this.setState({
      expanded: !this.state.expanded
    })
  }

  render() {
    const trackList = this.state.expanded 
      ? this.state.tracks.map((t, k) => (
          <Track t={t} key={k} />
        ))
      : (
        <button
          onClick={this.toggleTracks}
          className="expand">
            Visa avsnitt
        </button>
      );

    return (
      <div className="pod-tracklist">
        {trackList}
      </div>
    )
  }
}

export default TrackList;