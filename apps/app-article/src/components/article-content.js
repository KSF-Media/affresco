import React, {Component} from 'react';
import quoteIcon from "../assets/images/quotes-png-11.png";

class Content extends Component {
    constructor(props) {
        super(props);
    }

    conditionalRendering(block, key) {
        let blockType = Object.keys(block)[0].toLowerCase() || null;
        if (blockType != null) {
            if (blockType === 'paragraph') {
                return this.renderParagraph(block, key);
            } else if (blockType === 'headline') {
                return this.renderHeadline(block, key);
            } else if (blockType === 'image') {
                return this.renderImage(block, key);
            } else if (blockType === 'html') {
                return this.renderHtml(block, key);
            } else if (blockType === 'factbox') {
                return this.renderFactBox(block, key);
            } else if (blockType === 'quote') {
                return this.renderQuotes(block, key);
            }
        } else {
            console.log('couldn\'t render ' + {blockType});
        }
    }

    renderParagraph(block, key) {
        return (
            <p className={"paragraph"} key={key}>{htmlToReactParser.parse(block.paragraph)}</p>
        );
    }

    renderQuotes(block, key) {
        return (
            <div className={"quotePlaceHolder"} key={key}>
                <div className={"row"}>
                    <div className={"col-2 thojzat"}><img width="30px" src={quoteIcon} alt=""/></div>
                    <div className={"col-10 quote"} style={{paddingLeft: '0px'}}>
                        {block.quote}
                    </div>
                </div>
            </div>
        );
    }

    renderHeadline(block, key) {
        return (
            <h4 className={"headline"} key={key}>{block.headline}</h4>
        );
    }

    renderImage(block, key) {
        let appendBylineLabel = block.image.byline !== '' ? 'BILD:' : '';
        let caption = block.image.caption === null ? '' : block.image.caption;
        let byline = block.image.byline === null ? '' : block.image.byline;
        return (
            <div className={"image"} key={key}>
                <img className={"articleImage"} width="100%" src={block.image.url} alt=""
                     onClick={() => this.props.showHighResolutionImage(block.image.url, caption + " " + appendBylineLabel + " " + byline)}/>
                <p dangerouslySetInnerHTML={{__html: caption + " " + appendBylineLabel + " " + byline}}/>
            </div>

        );
    }

    renderFactBox(block, key) {
        return (
            <div className={"factBox"} key={key} id={"factBox-" + key}>
                <div className={"fakta"}>FAKTA</div>
                <h4>{block.factBox.title}</h4>
                <ul className={"factboxList"}>
                    {
                        block.factBox.content.map((fact, key) => {
                            return (
                                <li key={key} dangerouslySetInnerHTML={{__html: fact}}/>
                            )
                        })
                    }
                </ul>
                <div className={"expandOpacity"} id={"expandOpacity"}></div>
                <div className={"expandOpacity2"} id={"expandOpacity2"}></div>
                <div className={"expand"} id={"expandFactBox-" + key} onClick={() => {
                    this.expandFaktBox(key)
                }}>
                    <div style={{display:'inline-block'}}><span>VIK UT</span></div>
                    <div style={{display:'inline-block'}}><i className="arrow down"/></div>
                </div>
            </div>
        );
    }

    renderHtml(block, key) {
        return (
            <div className={"html"} key={key} dangerouslySetInnerHTML={{__html: block.html}}/>
            //    {/*<div className={"html"} key={key}>{htmlToReactParser.parse(block.html)}</div>*/} // youplay videos does not work
        );
    }

    expandFaktBox(key) {
        document.getElementById('factBox-' + key).style.height = "auto";
        document.getElementById('expandFactBox-' + key).style.display = "none";
        document.getElementById('expandOpacity').style.display = "none";
        document.getElementById('expandOpacity2').style.display = "none";
    }

    render() {
        return (
            <div className={"row"}>
                <div className={"col-sm-12 content text-left mt-2"} id={"content"}
                     style={{wordWrap: 'break-word'}}>
                    <div id="MOBPARAD"></div>
                    {
                        this.props.body != null ?
                            this.props.body.map((block, key) => {
                                return this.conditionalRendering(block, key)
                            })
                            :
                            ''
                    }
                </div>
            </div>
        )
    }
}

export default Content