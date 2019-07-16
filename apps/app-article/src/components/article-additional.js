import React, {Component} from 'react';
import shareIcon from "../assets/images/share.png";
import SvgIcon from "../svgIcon";
import soundIcon from "../assets/images/sound.png";


class Additional extends Component {
    constructor(props) {
        super(props);
    }

    componentDidMount() {

    }
    onShare() {
        Android.showIntent('https://ksfmedia.herokuapp.com/index.html?uuid=' + this.state.uuid);
    }

    render() {
        return(
            <div className={"row"}>
                <div className="col-8">
                    {/*{*/}
                    {/*    <a href="javascript:void(0)" onClick={() => {*/}
                    {/*        this.onShare()*/}
                    {/*    }} id={"shareLink"}>*/}
                    {/*        <img style={{marginTop: '4px'}} className={"shareIcon"} src={shareIcon}*/}
                    {/*             alt="Share"/></a>*/}
                    {/*}*/}
                </div>
                <div className="col-2">
                    {/*<span onClick={() => this.props.increaseFontSize()}> <SvgIcon name="textsize"/> </span>*/}
                </div>
                <div className="col-2">
                    <span onClick={() => this.props.increaseFontSize()}> <SvgIcon name="textsize"/> </span>
                    {/*<img src={soundIcon} width={"35px"} alt=""/>*/}
                </div>
            </div>
        )
    }
}

export default Additional
