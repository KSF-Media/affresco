import React from 'react';

class Track extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      expanded: false
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
    const trackDetails = this.state.expanded
      ? (
      <div>  
        <div className="media">
          <iframe src={"https://w.soundcloud.com/player/?url=https%3A//api.soundcloud.com/tracks/"+ t.id +"&color=%23ff5500&auto_play=false&hide_related=true&show_comments=false&show_user=false&show_reposts=false&show_teaser=false&show_artwork=false"}></iframe>
        </div>
        <div className="description">
          <img
            className="artwork"
            src={t.artwork_url}
          />
          {t.description}
        </div>
      </div>
      )
      : (
        <div>
          <img
            className="artwork"
            src={t.artwork_url}
          />
        </div>
      );
    return (
      <div
        onClick={this.toggleTrack}
        className="pod-track">
        <div className="pod-track-description">
          <h2 className="title">{t.title}</h2>
          {trackDetails}
          <div className="created">{t.created_at}</div>
        </div>
      </div>
    )
  }
}

export default Track;