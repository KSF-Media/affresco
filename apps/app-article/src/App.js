import React, {Component} from 'react';
import Lightbox from 'react-image-lightbox';
import {Login, logout} from '@ksf-media/user';
import articleApi from './article-service';
import 'react-image-lightbox/style.css';
import 'bootstrap-css-only/css/bootstrap.min.css';
import 'basscss/css/basscss-cp.css';
import {isUserLoggedIn, getUrlParam, getBrandValueParam, isDarkModeOn} from "./helper";
import hblDefaultImage from './assets/images/hbl-fallback-img.png';
import Header from "./components/header";
import Loading from "./components/loading";
import Additional from "./components/article-additional";
import PremiumBox from "./components/premium";
import ArticleDetails from "./components/article-details";
import Content from "./components/article-content";
import RelatedArticles from "./components/related-articles";
import Footer from "./components/footer";
import ManuallyRelatedArticles from "./components/manually-related-articles";
import Cookies from 'js-cookie';
import { AndroidView } from 'react-device-detect';

class App extends Component {
    constructor(props) {
        super(props);
        this.state = {
            uuid: null,
            title: null,
            mainImage: {
                url: hblDefaultImage,
                caption: null,
                byline: null,
            },
            authors: null,
            tags: [],
            premium: null,
            preamble: null,
            body: [],
            publishingTime: null,
            updateTime: null,
            category: null,
            articleTypeDetails: null,
            relatedArticles: [],
            shareUrl: null,
            infogram: {
                html: null
            },
            isLoading: false,
            cachedArticles: [],
            appearLogin: false,
            showBuyOption: false,
            user: null,
            fontSize: 1.06, // in rem
            fontSizeThreshold: 3,
            fontSizeIncrementalValue: 0.5,
            isImageModalOpen: false,
            modalImage: null,
            modalCaption: '',
            latestArticles: [],
            mostReadArticles: [],
            errorFetching: false,
            errorFetchingLatestArticles: false,
            forceLoginView: false
        };
    }
    componentDidMount() {
        if (localStorage.getItem("currentUser") !== null) {
            this.setState({user: JSON.parse(localStorage.getItem("currentUser"))});
        }
        if (Cookies.get("fontSize")) {
            this.setState({fontSize: parseFloat(Cookies.get("fontSize"))});
        }

        //In case User want to logout, the value should be false
        if(Cookies.get('LoginStatus') != undefined && Cookies.get('LoginStatus') === "false"){
            //we remove it to avoid infinite loop
            Cookies.remove('LoginStatus');
            //TODO
            // we need to have logout listener here, after success we can then remove the cookie, if for exemple an error happend while             
            //LogOut;in localstorage will keep logged but for android is considered as logged out

            logout(this.onLogout);
        }
        if(getUrlParam().has('logout')){
            logout(this.onLogout, (err) => Android.onLogoutFailed());
        }
        if(getUrlParam().has('login')){
            this.setState({forceLoginView: true});
        }else {
            this.getArticle();
            this.getMostReadArticles();
        }
    }
    componentDidUpdate() {
    }
    componentWillUnmount() {
    }  
    onLogout() {
        Android.onLogoutSuccess();
        console.log("Logged out successfully!");
        //Remove the current user from localstorage 
        localStorage.removeItem("currentUser");
        localStorage.removeItem("cachedArticles");
        Cookies.set('LoginStatus', false);
    }    
    getArticle() {
        let urlParams = getUrlParam();
        if (JSON.parse(localStorage.getItem('cachedArticles')) != null) {
            this.setState({cachedArticles: JSON.parse(localStorage.getItem('cachedArticles'))});
        }        
        if (urlParams.has('uuid')) {
            if (this.checkCache(urlParams.get('uuid'))) {
                this.fetchArticleFromCache(urlParams.get('uuid'));
            } else {
                this.fetchArticleFromApi(urlParams.get('uuid'));
            }
        }else {
            console.log("no uuid found!")
            // TODO:: handle this part
        }
    }

    getLatestArticles(){
        articleApi.getLatestArticles()
            .then(data => {
                this.setState({latestArticles: data})
            })
            .catch(error => {
                this.setState({isLoading: false});
                this.setState({isLoading: false, errorFetchingLatestArticles: true});
            });
    }

    getMostReadArticles(){
        articleApi.getMostReadArticles()
            .then(data => {
                this.setState({mostReadArticles: data})
            })
            .catch(error => {
                this.setState({isLoading: false});
                this.setState({isLoading: false, errorFetchingLatestArticles: true});
            });
    }

    checkCache(uuid) {
        if(!isUserLoggedIn()){
            try {
                let cachedArticles = localStorage.getItem('cachedArticles');
                if (cachedArticles != null) {
                    let parsedArticles = JSON.parse(cachedArticles)
                    return parsedArticles.find(article => {
                        return article.uuid === uuid
                    });
                }
            } catch (e) {
                console.log(e);
            }
        }
        return false;
    }

    cleanCache() {
        if (JSON.parse(localStorage.getItem('cachedArticles')) != null) {
            let newCache = [];
            JSON.parse(localStorage.getItem('cachedArticles')).map(article => {
                let now = new Date();
                let articleDate = new Date(article.timestamp);
                let timeDiff = Math.abs(articleDate.getTime() - now.getTime());
                let timeDiffInHour = ((timeDiff / (1000 * 60 * 60)) % 24);
                if (timeDiffInHour <= 0.1) { // clean cache if article is older than x hours
                    newCache.push(article);
                }
            });
            localStorage.setItem('cachedArticles', JSON.stringify(newCache));
        }
    }

    fetchArticleFromCache(uuid) {
        if (JSON.parse(localStorage.getItem('cachedArticles')) != null) {
            JSON.parse(localStorage.getItem('cachedArticles')).map((article, index) => {
                if (article.uuid === uuid) {
                    this.setState({
                        mainImage: article.mainImage,
                        uuid: article.uuid,
                        title: article.title,
                        authors: article.authors,
                        tags: article.tags,
                        premium: article.premium,
                        category: article.articleType,
                        preamble: article.preamble,
                        body: article.body,
                        relatedArticles: article.relatedArticles,
                        publishingTime: article.publishingTime,
                        updateTime: article.updateTime,
                        shareUrl: article.shareUrl,
                        articleTypeDetails: article.articleTypeDetails
                    }, () => {
                        if (article.externalScripts != null) {
                            this.appendThirdPartyScript(article.externalScripts);
                        }
                        // this.positionAdsWithinArticle(); todo:: method needs to be fixed
                        this.resizeText(this.state.fontSize);
                        document.title = this.state.title;
                        this.pushLoadingArticleToGoogleTagManager(article);

                        this.cleanCache();
                    });
                } else {
                    console.log("article not found from cache");
                }
            })
        }
    }

    fetchArticleFromApi(uuid) {
        this.setState({isLoading: true});
        articleApi.getArticle(uuid)
            .then(data => {
                if (data.http_code === 400) {
                    this.setState({isLoading: false, showBuyOption: true});
                } else if (data.http_code === 403) {
                    this.setState({
                        isLoading: false,
                        showBuyOption: true,
                        appearLogin: false,
                        mainImage: data.not_entitled.articlePreview.mainImage,
                        uuid: data.not_entitled.articlePreview.uuid,
                        title: data.not_entitled.articlePreview.title,
                        category: data.not_entitled.articlePreview.articleType,
                        authors: data.not_entitled.articlePreview.authors,
                        tags: data.not_entitled.articlePreview.tags,
                        preamble: data.not_entitled.articlePreview.preamble,
                        premium: data.not_entitled.articlePreview.premium,
                        body: data.not_entitled.articlePreview.body,
                        relatedArticles: data.not_entitled.articlePreview.relatedArticles,
                        publishingTime: data.not_entitled.articlePreview.publishingTime,
                        updateTime: data.not_entitled.articlePreview.updateTime,
                        shareUrl: data.not_entitled.articlePreview.shareUrl,
                        articleTypeDetails: data.not_entitled.articlePreview.articleTypeDetails
                    }, () => {
                        this.resizeText(this.state.fontSize);
                        if (data.not_entitled.articlePreview.externalScripts != null) {
                            this.appendThirdPartyScript(data.not_entitled.articlePreview.externalScripts);
                        }
                    });
                } else {
                    this.setState({
                        mainImage: data.mainImage,
                        uuid: data.uuid,
                        title: data.title,
                        authors: data.authors,
                        category: data.articleType,
                        tags: data.tags,
                        premium: data.premium,
                        preamble: data.preamble,
                        body: data.body,
                        relatedArticles: data.relatedArticles,
                        publishingTime: data.publishingTime,
                        updateTime: data.updateTime,
                        shareUrl: data.shareUrl,
                        articleTypeDetails:data.articleTypeDetails
                    }, () => {
                        if (data.externalScripts != null) {
                            this.appendThirdPartyScript(data.externalScripts);
                        }
                        document.title = this.state.title;
                        this.pushLoadingArticleToGoogleTagManager(data);
                        this.positionAdsWithinArticle();
                    });
                    this.setState({isLoading: false, showBuyOption: false, appearLogin: false});
                    data.timestamp = new Date().getTime();
                    this.setState({cachedArticles: this.state.cachedArticles.concat(data)}, () => {
                        if (!this.checkCache(data.uuid)) {
                            localStorage.setItem('cachedArticles', JSON.stringify(this.state.cachedArticles));
                        }
                    });
                    this.cleanCache();
                }
                this.resizeText(this.state.fontSize);
            })
            .catch(error => {
                this.setState({isLoading: false, errorFetching: true});
            });
    };

    loadScriptError(oError) {
         console.log('error: ', oError);
    }

    loadScriptSuccess(res) {
         console.log('success: ', res);
    }

    // not the best solution but seems is working for now, delay the appending of script for each element in array
    appendThirdPartyScript(externalScriptArray) {
        const delayTime = 800;
        externalScriptArray.forEach((script, index) => {
            setTimeout(() => {
                this.appendNewTagElementToDom(script);
            }, delayTime * index);
        });
    }

    appendNewTagElementToDom(script) {
        // Create an element outside the document to parse the string with
        const head = document.createElement("head");

        // Parse the string
        head.innerHTML = script;

        // Copy those nodes to the real `head`, duplicating script elements so
        // they get processed
        let node = head.firstChild;
        while (node) {
            const next = node.nextSibling;
            if (node.tagName === "SCRIPT") {
                // Just appending this element wouldn't run it, we have to make a fresh copy
                const newNode = document.createElement("script");
                if (node.src) {
                    newNode.src = node.src;
                }
                while (node.firstChild) {
                    // Note we have to clone these nodes
                    newNode.appendChild(node.firstChild.cloneNode(true));
                    node.removeChild(node.firstChild);
                }
                node = newNode;
                newNode.onload = this.loadScriptSuccess;
                newNode.onerror = this.loadScriptError;

            }
            document.head.appendChild(node);
            node = next;
        }
    }

    pushLoadingArticleToGoogleTagManager(article) {
        let push_data = {'event': 'page_data'};

        if (this.state.user && typeof this.state.user.uuid != 'undefined') {
            push_data.userid = this.state.user.uuid;
            push_data.cusno = this.state.user.cusno;
        }

        if (this.state.user && this.state.user.subs) {
            let packages = [];
            for (let i in this.state.user.subs) {
                if (this.state.user.subs[i].state === "Active" && packages.indexOf(this.state.user.subs[i].package.id) === -1) {
                    packages.push(this.state.user.subs[i].package.id)
                }
            }
            push_data.packageid = packages.sort().toString();
        }

        if (typeof article == 'object') {

            let authors = [];
            article.authors.map(author =>{
                authors.push(author.byline);
            });

            push_data.authors = authors;
            push_data.category = article.articleType;
            push_data.brand = getBrandValueParam() + '.fi';
            push_data.tags = article.tags;
            push_data.publish_date = article.publishingTime;
            push_data.update_date = article.publishingTime;
            push_data.content_id = article.uuid;
            push_data.is_authenticated = isUserLoggedIn();
            push_data.is_premium = article.premium ? 'PREMIUM' : 'FREE';
            push_data.url = article.shareUrl;
            push_data.analyticsCategory = article.analyticsCategory;
            push_data.analyticsSection = article.analyticsSection;
            push_data.app_os = navigator.userAgent.match(/Android/) ? "Android" : "iOS";
            push_data.articlePriority = article.articlePriority;
        }

        window.dataLayer.push(push_data);
    }

    /* The purpose of this function is to place ad slots in page text. It will try to space them out avoiding images, news graphics, iframes etc. 
    It will not place slots in articles with less than 4 paragraphs. If no good paragraphs are found, the slots are placed after the text. It will allow <b>, <a href> and <i> within paragraphs. Other tags will disqualify that paragraph. Note that this function will call the main ads script on completion. This should prevent timing issues, no ads are loaded till all paragraphs are rolled out and inspected.
    The script depends on the article text residing in an element with the id 'content'. */
    positionAdsWithinArticle() {
        let textParagraphNum = 0; // incremental number of text paragraphs
        let textParagraphsOK = []; // storage for list of good paragraphs. We will use this array to find good spots for our slots.
        let textParagraphCount = 0; // count of paragraph array groups
        let slotOne = '<div id="DIGIHELMOB"></div>';
        let slotTwo = '<div id="MOBMITT"></div>';
        textParagraphsOK.push([]);

        var contentDiv = document.getElementById('content');
        if (contentDiv != null) {
            contentDiv.childNodes.forEach(node => {

                    if (node.className === 'html') {
                        var OK = true;
                        let approvedTags = ['B', 'A', 'I'];
                        let tt = node.getElementsByTagName("*");
                        // try to support iPads from 2013. They cannot iterate browser objects only js arrays.
                        var t = Array.prototype.slice.call(tt);
                        // check for non-text content
                        if (t.length > 0) {
                            for (let item of t) {
                                let upperCased = item.nodeName;
                                upperCased = upperCased.toUpperCase();

                                if (approvedTags.indexOf(upperCased) > -1) {} else {
                                    OK = false;
                                }
                            }
                        }
                                        textParagraphNum++;
                                        if (OK && textParagraphNum > 3) {
                                            // We are far enough from the beginning and the previous para was text, as is this and text is long enough. Good! We approve the paragraph for ads 
                                            textParagraphsOK[textParagraphCount].push(textParagraphNum);
                                        } else {
                                            textParagraphCount++;
                                            textParagraphsOK.push([]);
                                        }
                                        } else {
                                            textParagraphCount++;
                                            textParagraphsOK.push([]);
                                        }
                                        });

                                        }
textParagraphsOK = textParagraphsOK.filter(set => set.length > 0);
var digiHelPick = 0;
var digiHelGood = false;
var mobMittPick = 0;
var mobMittGood = false;
var AllParas = contentDiv.getElementsByClassName('html');
if (AllParas && textParagraphsOK.length > 0) {
    digiHelPick = textParagraphsOK[0][0];
    if (digiHelPick > 3) {
        digiHelGood = true;
        AllParas[digiHelPick - 1].insertAdjacentHTML('beforebegin', slotOne);
    }
    let mMittSet = Math.ceil(textParagraphsOK.length / 2);
    if (mMittSet > 1 || textParagraphsOK[mMittSet - 1].length > 5) {
        mobMittPick = textParagraphsOK[mMittSet - 1][Math.floor(textParagraphsOK[mMittSet - 1].length / 2)];
        mobMittGood = true;
        AllParas[mobMittPick - 1].insertAdjacentHTML('beforebegin', slotTwo);
    }
    // Place slots after text in case we could not place them in text.
    if (!digiHelGood) {
        let endPlace = AllParas.length - 1;
        AllParas[endPlace].insertAdjacentHTML('afterend', slotOne);
    }
    if (!mobMittGood) {
        let endPlace = AllParas.length - 1;
        AllParas[endPlace].insertAdjacentHTML('afterend', slotTwo);
    }
}

if (window.ksfDfp) {
    window.ksfDfp.startUp();
}
}

    onRegisterOpen() {
        // alert("register opn .");
    }

    onUserFetchSuccess(user) { 
        Cookies.set('LoginStatus', true, { expires: 365 });
        Cookies.set('token', localStorage.getItem('token'), { expires: 365 });
        Cookies.set('uuid', localStorage.getItem('uuid'), { expires: 365 });
        //To get User data from Android side 
        Cookies.set('currentUser', JSON.stringify({ firstName: user.firstName, lastName: user.lastName, email: user.email, token: localStorage.getItem('token'), uuid: localStorage.getItem('uuid') }, {expires: 365}));
        localStorage.setItem("currentUser", JSON.stringify(user));
        this.setState({user: user});
        const articleUuid = getUrlParam().get('uuid');
        if (articleUuid) { 
            this.fetchArticleFromApi(articleUuid);
        }
        // Call Android bridge 
        try {
            Android.isLoggedIn();
        } catch (e) {
            console.error('Android not defined');
        }
    }

    onUserFetchFail(error){

    }

    showLogin = (e) => {
        e.preventDefault();
        this.setState({appearLogin: true, showBuyOption: false}, () => {
            document.getElementById('loginForm').scrollIntoView();
        });
    };

    increaseFontSize = () => {
        let increaseTextSize = parseFloat(this.state.fontSize) + parseFloat(this.state.fontSizeIncrementalValue);
        this.setState({fontSize: increaseTextSize}, () => {
            Cookies.set("fontSize", this.state.fontSize, { expires: 365 });
            if (this.state.fontSize > this.state.fontSizeThreshold) {
                Cookies.set("fontSize", "1", { expires: 365 });
                this.setState({fontSize: 1}, () => {
                    this.resizeText(1);
                });
            }
            this.resizeText(this.state.fontSize);
        });
    };

    resizeText(newSize) {
        if (document.getElementsByClassName('title').length > 0) {
            const articleTitle = document.getElementsByClassName('title')[0];
            articleTitle.style.fontSize = newSize + 1 + "rem";
            articleTitle.style.lineHeight = "100%";
        }

        if (document.getElementsByClassName('preamble').length > 0) {
            const articleTitle = document.getElementsByClassName('preamble')[0];
            articleTitle.style.fontSize = newSize  + 0.05 + "rem";
            articleTitle.style.lineHeight = "120%";
        }

        const nodes = document.querySelectorAll('#content');
        nodes.forEach(a => {
            a.style.fontSize = newSize + "em";
        });

        let articleHeadings = document.getElementsByClassName("headline");
        for (let i = 0; i < articleHeadings.length; i++) {
            let heading = document.getElementsByClassName("headline")[i];
            heading.style.fontSize = newSize + 0.4 + "rem";
        }
    }

    showHighResolutionImage = (imgSrc, caption) => {
        this.setState({isImageModalOpen: true, modalImage: imgSrc, modalCaption: caption})
    };

    render() {
        let appendBylineLabel = '';
        let caption = '';
        let byline = '';

        if (this.state.mainImage != null) {
            appendBylineLabel = this.state.mainImage.byline !== '' ? 'BILD:' : '';
            caption = this.state.mainImage.caption === null ? '' : this.state.mainImage.caption;
            byline = this.state.mainImage.byline === null ? '' : this.state.mainImage.byline;
        }

        const {isImageModalOpen} = this.state;

        if (this.state.errorFetching) {
            return <ErrorPage message={"Laddar..."}/>;
        }
        
        if(this.state.forceLoginView){
            return <Login onRegister={() => this.onRegisterOpen()} onUserFetchSuccess={(user) => this.onUserFetchSuccess(user)} onUserFetchFail={(error) => this.onUserFetchFail(error)} disableSocialLogins={["Facebook", "Google"]}/>;
        }

        return (
            <div className="App">
                {this.state.isLoading ? <Loading/>:''}
                                
                {isImageModalOpen && (
                    <Lightbox
                        mainSrc={this.state.modalImage + '&width=1200'}
                        onCloseRequest={() => this.setState({isImageModalOpen: false})}
                        // imageTitle={this.state.title}
                        imageCaption={this.state.modalCaption}
                        enableZoom={true}
                    />
                )}

                <div className={`container-fluid article ${isDarkModeOn() ? 'darkMode': ''} `}>
                    <React.Fragment>
                        <Tag tags={this.state.tags}/>
                        {
                            this.state.category === 'Advertorial' ? 
                                <div>
                                    <div className={"row"}>
                                        <div class="advertorial-top-box">
                                            <div class="advertorial-top-box-left">ANNONS</div>
                                        </div>
                                    </div>
                                </div>
                            :   ''
                        }
                        <Title title={this.state.title}/>
                        <Header showHighResolutionImg={this.showHighResolutionImage} mainImage={this.state.mainImage}
                                caption={caption} appendBylineLabel={appendBylineLabel} byline={byline}/>
                        <Additional preamble={this.state.preamble} increaseFontSize={this.increaseFontSize}/>
                        <ArticleDetails category={this.state.category} premium={this.state.premium}
                                        authors={this.state.authors} publishingTime={this.state.publishingTime}
                                        updateTime={this.state.updateTime} articleTypeDetails={this.state.articleTypeDetails}/>
                        <Content body={this.state.body}
                                 showHighResolutionImage={this.showHighResolutionImage}/>
                        <div className={"row"}>
                            <div className={"col-sm-12"}>
                                {
                                    this.state.showBuyOption ?
                                        <PremiumBox showLogin={this.showLogin}/>
                                        :
                                        ""
                                }

                                {
                                    this.state.appearLogin ?
                                        <div id={"loginForm"}><strong>Logga in: </strong></div>
                                        :
                                        ""
                                }

                                {
                                    this.state.appearLogin ?
                                        <Login onRegister={() => this.onRegisterOpen()}
                                               onUserFetchSuccess={(user) => this.onUserFetchSuccess(user)} onUserFetchFail={(error) => this.onUserFetchFail(error)} disableSocialLogins={["Facebook", "Google"]}/>
                                        :
                                        ""
                                }
                            </div>
                        </div>
                        {
                            this.state.relatedArticles.length > 0 ?
                                <ManuallyRelatedArticles manuallyRelatedArticles={this.state.relatedArticles}/>
                                :
                                ''
                        }
                        <div id="MOBNER"></div>
                        <RelatedArticles relatedArticles={this.state.mostReadArticles}/>
                    </React.Fragment>
                </div>
                {/*<div id="MOBMITT"></div>*/}

                <Footer brandValueName={getBrandValueParam()}/>
            </div>
        );
    }
}

const ErrorPage = (props) => {
    return (
        <div className={"row"}>
            <div className={"col-12 mt-2 mt-5 text-center"} style={{wordWrap: 'break-word'}}>
                <h2 className={"title"}>{props.message}</h2>
            </div>
        </div>
    )
};

const Title = (props) => {
    return (
        <div className={"row"}>
            <div className={"col-12 mt-2 mb-3"} style={{wordWrap: 'break-word'}}>
                <h2 className={`title ${isDarkModeOn() ? 'darkMode': ''}`}>{props.title}</h2>
            </div>
        </div>
    )
};

const Tag = (props) => {
    let tags = props.tags;
    let tag = '';
    if (tags.length > 0) {
        tag = tags[0];
    }

    return (
        <div className={"row"}>
            <div className={`col-12 mt-2 mb-1 articleTag brandColor-${getBrandValueParam()}`}>
                {tag}
            </div>
        </div>
    )
};

export default App;
