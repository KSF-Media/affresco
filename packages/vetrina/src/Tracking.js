window.dataLayer = window.dataLayer || [];

function Tracker() {
    this.dataLayer = window.dataLayer;
}

exports.newTracker = function () {
    return new Tracker();
}

exports.transaction = function (tracker) {
    return function () {
        referringArticle = window.location.href.split('?')[0];
        tracker.dataLayer.push({
            'event': 'transaction',
            'ecommerce': {
                'purchase': {
                    'actionField': {
                        'id': 'testid', // Transaction ID. Required for purchases and refunds.
                        'affiliation': referringArticle, // The store or affiliation from which this transaction occurred
                        'revenue': 6.90, // Total transaction value (incl. tax and shipping)
                        'tax': '',
                        'shipping': '',
                        'coupon': ''
                    },
                    'products': [{
                        'name': "HBL Premium",
                        'id': "testid", // TODO: we should get this id as "campaign code" from kayak subscription->campaign
                        'price': 6.90,
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