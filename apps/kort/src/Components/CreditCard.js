export function openLocation(location) {
    return function() {
	console.log("location to "+location);
	window.location = location;
    }
}
