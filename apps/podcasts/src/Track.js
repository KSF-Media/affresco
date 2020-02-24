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
          <div className="track-details-media">
            <iframe src={"https://w.soundcloud.com/player/?url=https%3A//api.soundcloud.com/tracks/"+ t.id +"&color=%23ff5500&auto_play=false&hide_related=true&show_comments=false&show_user=false&show_reposts=false&show_teaser=false&show_artwork=false"}></iframe>
          </div>
          <div className="track-details-description">
            {t.description}
          </div>
        </div>
      )
      : (
        <div></div>
      );
    return (
      <div
        onClick={this.toggleTrack}
        className="pod-track">
        <div>
          <div className="pod-track-artwork">
            <img
                className="artwork"
                src={t.artwork_url}
            />
          </div>  
          <div className="pod-track-description">
            <div className="pod-podname">{t.user.username}</div>
            <h2 className="title">{t.title}</h2>
            <div className="created">{t.created_at}</div>
          </div>
        </div>
        <div>{trackDetails}</div>
      </div>
    )
  }
}

export default Track;