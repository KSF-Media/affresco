window.dataLayer = window.dataLayer || [];

function Tracker() {
    this.dataLayer = window.dataLayer;
}

exports.transaction_ = function (orderNumber, productId, productPriceInCents) {
    return function () {
        var tracker = new Tracker();
        var referringArticle = window.location.href.split('?')[0];
        var productPrice = productPriceInCents / 100;
        var brand;
        switch (document.location.hostname) {
            case "www.vastranyland.fi":
                brand = "VN";
                break;
            case "www.ostnyland.fi":
                brand = "ON"
                break;
            case "www.hbl.fi":
                brand = "HBL"
                break;
            default:
                brand = ""
        }
        tracker.dataLayer.push({
            'event': 'transaction',
            'ecommerce': {
                'purchase': {
                    'actionField': {
                        'id': orderNumber, // Transaction ID. Required for purchases and refunds.
                        'affiliation': referringArticle, // The store or affiliation from which this transaction occurred
                        'revenue': productPrice, // Total transaction value (incl. tax and shipping)
                        'tax': '',
                        'shipping': '',
                        'coupon': ''
                    },
                    'products': [{
                        'name': productId,
                        'id': productId, // TODO: we should get this id as "campaign code" from kayak subscription->campaign
                        'price': productPrice,
                        'brand': "HBL",
                        'category': 'Magazines & Newspapers',
                        'variant': '',
                        'quantity': 1
                    }]
                }
            }
        })
    }
};
