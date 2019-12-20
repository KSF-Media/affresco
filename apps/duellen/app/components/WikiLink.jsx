import React from 'react';
import $ from 'jquery';

export default class WikiLink extends React.Component{
    constructor(props){
        super(props);
        this.state = {
          wikiresponse:[{
            title: '',
            description: '',
          }],
          oldSearch: '',       
        }
      };
    // every time something new is typed in to the input field this will search for it in wikipedia
    componentDidUpdate(){
      // Needs tho have this so it won't update all of the time and it won't search for an empty search 
      if (this.props.search !== this.state.oldSearch && this.props.search !== ""){
        this.setState({
          oldSearch: this.props.search
        })
        $.ajax({
        url: "https://sv.wikipedia.org/w/api.php?action=opensearch&format=json&suggest=true&formatversion=2&search=" + this.props.search,
        dataType: 'jsonp',
        success: response => {
          // returns a list of indexes for aka how many search results we got from the wiki search 
          var pagesFiltered = Object.keys(response[1]).filter(function(key) {
              return [key]
            })
          // wikiresponse needs to be an array for the map funktion
          // But has an dictionary for every search result
          this.setState({
            wikiresponse: pagesFiltered.map(function(key){
              return {title: response[1][key], description: response[2][key]}
            })
          })
        }
      })
    }
    }

  render(){
    // The map funktion returns an error if ther is an empty array 
    // And we don't want anything to show it there hasn't been a search for anything
    if(this.props.search){
      return(
          <div className="w-100">
            {this.state.wikiresponse.map(({title, description}) => (
              <button key={title} onClick={() => this.props.onClick(title)} type="button" className="w-100 p-3 mb-2 bg-light text-black border">
                    <b>{title}</b>
                    {/* Steffe wanted pepole to test with only the title
                    <br/>
                    {description}
                    */}
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

