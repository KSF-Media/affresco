import React from 'react';

import Track from './Track.js'

class LatestTracksList extends React.Component {
  constructor(props){
    super(props);
    this.state = {
      expanded: false
    }
    this.toggleTracks = this.toggleTracks.bind(this);
  }

  toggleTracks() {
    this.setState({
      expanded: !this.state.expanded
    })
  }

  render() {
    const trackList = this.props.tracks.map((t, k) => (
      <Track t={t} key={k} />
    ));

    return (
      <div className="pod-tracklist">
        {trackList}
      </div>
    )
  }
}

export default LatestTracksList;