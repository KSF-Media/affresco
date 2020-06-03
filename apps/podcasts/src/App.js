import React from "react";

import Track from "./Track.js";
import "./less/main.less"

class App extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      podcastIds: window.pod_ids || process.env.PODCAST_IDS.split(',') || [],
      podAppUlr: window.pod_app_url || '',
      podcasts: [],
      tracks: [],
      selectedPodcast: "",
      loading: null,
      loadedPodcasts: null
    }
  }

  componentDidMount() {
    this.setState({
      loading: true,
      loadedPodcasts: 0
    });
    const podRequests = this.state.podcastIds.map(id => 
      fetch(process.env.NODE_ENV === 'development'
        ? `http://localhost:9000/getPodcast?id=${id}`
        : `${this.state.podAppUlr}/.netlify/functions/getPodcast?id=${id}`
      ).then(res => {
        this.setState({loadedPodcasts: this.state.loadedPodcasts+1});
        return res.json()
      })
    );
    Promise.all(podRequests).then(jsons => {
      jsons.map(json => {
        let channel = json.rss.channel;
        let tracks = channel.item || [];
        let podcast = channel.title._text || null;
        this.setState({
          tracks: this.state.tracks.concat(tracks.map(t => {
            return {...t, podcast};
          })),
          podcasts: this.state.podcasts.concat(podcast),
          loading: false
        });
      });
    });
  }

  // Select podcast
  selectPodcast = (name) => {
    this.setState({
      selectedPodcast: name
    })
  }

  render() {
    // JSX: Reset button to show all podcasts
    const resetListButton = this.state.selectedPodcast
      ? <li onClick={() => {this.setState({selectedPodcast: ""})}}>Se alla</li>
      : null;
    // JSX: Podcasts nav menu
    const podcastNav = this.state.podcasts.length > 1
      ? <nav className="pod-nav">
          <div className="label">Välj podd</div>
          <ul className="nav nav-lg">
            {
              this.state.podcasts.map((name, key) => {
                let buttonClass = (this.state.selectedPodcast === name || !this.state.selectedPodcast) ? 'active' : '';
                return <li className={buttonClass} onClick={() => this.selectPodcast(name)} key={key}>{name}</li>
              })
            }
            {resetListButton}
          </ul>
        </nav>
      : null

    // JSX: Loading indicator
    const spinner = this.state.loading === true
      ? <div className="loading">
          <p>Laddar</p>
          <div className="progress">
            <div
              className="progress-bar"
              style={{width: `${100 * this.state.loadedPodcasts / this.state.podcastIds.length}%`}}>
            </div>
          </div>
        </div>
      : null;

    // Filter tracks by selected podcast
    const filteredTracks = this.state.selectedPodcast  
      ? this.state.tracks.filter(track => track.podcast === this.state.selectedPodcast)
      : this.state.tracks;
    // Sorts tracks by pubDate
    const sortedTracks = filteredTracks.sort((a,b) => {
      const x = new Date(a.pubDate._text).getTime();
      const y = new Date(b.pubDate._text).getTime();
      return y - x;
    });
    // JSX: List of tracks
    const trackList = sortedTracks.map((t, k) => {
      let id = t.guid._text.split('/')[1];
      return <Track t={t} id={id} key={id} selectPodcast={this.selectPodcast} expanded={false} />
    });

    return (
      <div className="pod-app">
        {podcastNav}
        {spinner}
        <div className="pod-tracklist">
          {trackList}
        </div>
      </div>
    )
  }
}

export default App;
