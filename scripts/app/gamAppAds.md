# Documentation

The GPT file from Google.

```html
<script
  async="async"
  src="https://www.googletagservices.com/tag/js/gpt.js"
></script>
```

The script itself from wherever it is hosted.

```html
<script async="async" src="gamAppAds.js"></script>
```

Div tags in the HTML should(???) correspond to the ones known by the script.

The script is looking for divs in the HTML based on ids like these:

- MOBPARAD
- MOBNER
- DIGIHELMOB

These names are not final(???). The expected HTML is this:

```html
<div id="string"></div>
```

## GDPR

GPDR consent is hard-coded as using the app will require consent.

The code listens for ads marked `newspaper > app` and there is an order set up in Google Ad Manager to populate the app during testing.

In production there will presumably be three values for newspaper, one for each paper.

The div tags above currently all support these sizes:

- 300 x 100
- 300 x 250
- 300 x 300
- 300 x 600

The sizes are also not final.

`onSwitch` is hard-coded as true at the moment. Should probably be set according to something it can listen for. There is a requirement for this functionality but it cannot be provided by Google Ad Manager. So it has to be set here.
