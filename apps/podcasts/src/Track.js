import React from 'react';

class Track extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      expanded: props.expanded
    }
    this.toggleTrack = this.toggleTrack.bind(this);
  }

  toggleTrack() {
    this.setState({
      expanded: !this.state.expanded
    });
  }

  render() {
    const t = this.props.t;
    const cssClass = this.state.expanded ? 'pod-track expanded' : 'pod-track';
    console.log(this.props.key);
    const trackUrl = `https://w.soundcloud.com/player/?url=https%3A//api.soundcloud.com/tracks/${this.props.id}&color=%23ff5500&auto_play=true&hide_related=true&show_comments=false&show_user=false&show_reposts=false&show_teaser=false&show_artwork=false`;
    const trackDescription = t.description._text.split('\n').map((p, key) => <p key={key}>{p}</p>)
    const trackDetails = this.state.expanded
      ? (
        <div>  
          <div className="track-details-media">
            {/*
            <audio
              type={t.enclosure._attributes.type}
              src={t.enclosure._attributes.url}
              controls
              autoPlay
            />
            */}
            <iframe 
              width="100%"
              height="166"
              scrolling="no"
              frameborder="no"
              allow="autoplay"
              src={trackUrl}>
            </iframe>
          </div>
          <div className="track-details-description">
            {trackDescription}
          </div>
        </div>
      )
      : null;

    return (
      <div
        className={cssClass}>
        <div>
          <div className="pod-track-artwork">
            <img
                className="artwork"
                src={t['itunes:image']._attributes.href}
            />
          </div>  
          <div className="pod-track-description">
            <div>
              <div
                className="pod-podname"
                onClick={() => this.props.selectPodcast(t.podcast)}
              >
                {t.podcast  }
              </div>
              <h2  onClick={this.toggleTrack} className="title">{t.title._text}</h2>
              <div className="created">{t.pubDate._text}</div>
            </div>
            <div>
              <a onClick={this.toggleTrack}>
                <img className="pod-play" src="https://cdn.ksfmedia.fi/icons/play-button.png" />
              </a>
            </div>
          </div>
        </div>
        <div>{trackDetails}</div>
      </div>
    )
  }
}

export default Track;
