function pushLoadingArticleToGoogleTagManager(article, user) {
  let push_data = { event: "page_data" };
  const isUserDefined = user && typeof user.uuid != "undefined";

  if (isUserDefined) {
    push_data.userid = user.uuid;
    push_data.cusno = user.cusno;
  }

  if (user && user.subs) {
    let packages = [];
    for (let i in user.subs) {
      if (
	user.subs[i].state === "Active" &&
	packages.indexOf(user.subs[i].package.id) === -1
      ) {
	packages.push(user.subs[i].package.id);
      }
    }
    push_data.packageid = packages.sort().toString();
  }

  if (typeof article == "object") {
    let authors = [];
    article.authors.map((author) => {
      authors.push(author.byline);
    });

    push_data.authors = authors;
    push_data.category = article.articleType;
    push_data.brand = article.paper + ".fi";
    push_data.tags = article.tags;
    push_data.publish_date = article.publishingTime;
    push_data.update_date = article.publishingTime;
    push_data.content_id = article.uuid;
    push_data.is_authenticated = isUserDefined;
    push_data.is_premium = article.premium ? "PREMIUM" : "FREE";
    push_data.url = article.shareUrl;
    push_data.teaser_headline = article.listTitle;
    push_data.analyticsCategory = article.analyticsCategory;
    push_data.analyticsSection = article.analyticsSection;
    push_data.app_os = navigator.userAgent.match(/Android/) ? "Android" : "iOS";
    push_data.articlePriority = article.articlePriority;
  }

  window.dataLayer.push(push_data);
}

window.pushLoadingArticleToGoogleTagManager =
  pushLoadingArticleToGoogleTagManager;
