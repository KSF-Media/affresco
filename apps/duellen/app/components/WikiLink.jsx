import React from 'react';
import $ from 'jquery';
import {withRouter} from 'react-router';
import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider';

export default class WikiLink extends React.Component{
    constructor(props){
        super(props);
        this.state = {
          wikiresponse:[{
            title: '',
            description: '',
            url: '',
          }],
          oldSearch: '',       
        }
      };

    componentDidUpdate(){
      if (this.props.search !== this.state.oldSearch && this.props.search !== ""){
        this.setState({
          oldSearch: this.props.search
        })
        $.ajax({
        url: "https://sv.wikipedia.org/w/api.php?action=opensearch&format=json&suggest=true&formatversion=2&search=" + this.props.search,
        dataType: 'jsonp',
        success: response => {
          var pagesFiltered = Object.keys(response[1]).filter(function(key) {
              return [key]
            })
          this.setState({
            wikiresponse: pagesFiltered.map(function(key){
              return {title: response[1][key], description: response[2][key], url: response[3][key]}
            })
          })
        }
      })
    }
    }

  render(){
    if(this.props.search){
   return(
      <div>
        {this.state.wikiresponse.map(({title, description, url}) => (
          <button key={title} onClick={() => this.props.onClick(title)} type="button" className="p-3 mb-2 bg-light text-black border">
              <p><b><a href={url} target="_blank">{title}</a></b>
                  <br/>
                  {description}
              </p>
          </button>
        ))}
    </div>
    );
  }else{
    return(
      <div>

      </div>
    )
  }
};
}

