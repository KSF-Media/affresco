import { applyCredentials } from '../Mosaico.Cache.Pubsub.Init/index.js';

// Side effect: May alter process.env.GOOGLE_APPLICATION_CREDENTIALS
const projectId = applyCredentials();
const paper = process.env.PAPER || "hbl"
const topic = 'aptoma-frontpage-'+paper;

// For developer convenience's sake, outside of deployments you most
// likely don't want this unless you're doing development on the
// caching layer.  Loading the library would crash Mosaico if the
// credentials were not available.
const PubSub = !!projectId ? require('@google-cloud/pubsub').PubSub : null;

export const enabled = !!projectId;

export function subscribeImpl(callback) {
    return async function() {
	const pubsub = new PubSub({projectId});
	const [subscription] = await pubsub
	      .topic(topic)
	      .createSubscription('mosaico-'+paper+"-"+Math.round(new Date().getTime()/1000)+
				  "-"+(Math.random().toString().substring(2)),
				  {expirationPolicy: { ttl: {seconds: 7*86400 }}});
	subscription.on('message', message => {
	    callback(message)();
	})
    };
};

export function categoryImpl(message){
    if (message.attributes.category) {
	return message.attributes.category;
    }
    return null;
};

export function content(message) {
    return message.data.toString();
};

export function maxAge(message) {
    return message.attributes.maxAge;
}
