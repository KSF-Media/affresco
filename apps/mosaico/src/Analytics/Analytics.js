exports._pushToDataLayer = function(metadata) {
    window.dataLayer = window.dataLayer || [];
    let push_data = { event: "page_data" };
    push_data.title = metadata.title
    push_data.publishingTime =  metadata.publishingTime
    push_data.authors = metadata.authors.split(", ")
    push_data.premium = metadata.premium
    push_data.listTitle = metadata.title
    push_data.category = metadata.category
    push_data.articleUuid = metadata.articleUuid
    push_data.tags = metadata.tags.split("\, ").map(x => x.replaceAll('"', ''))
    dataLayer.push(push_data)
}
