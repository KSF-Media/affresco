export default function insertAds(articleBody) {
  var bodyElems = articleBody.map((elem) => Object.keys(elem));
  var elemsCount = bodyElems.length;

  var ad1RoughPosition;
  var ad2RoughPosition;

  if (elemsCount > 15) {
    ad1RoughPosition = Math.floor(elemsCount / 3.3);
    ad2RoughPosition = Math.floor(elemsCount / 1.6);
  } else if (elemsCount > 6) {
    ad1RoughPosition = Math.floor(elemsCount / 2);
  }

  function findExactPositionForAd(roughPosition) {
    var position;
    for (let i = roughPosition; i < elemsCount; i++) {
      if (canAdGoHere(i)) {
        position = i;
        break;
      }
    }
    return position;
  }

  function canAdGoHere(i) {
    if (bodyElems[i - 1] == "html" && bodyElems[i] == "html") {
      return true;
    } else {
      return false;
    }
  }

  var ad1Position = findExactPositionForAd(ad1RoughPosition);
  var ad2Position = findExactPositionForAd(ad2RoughPosition);

  if (ad1Position && ad2Position) {
    // body = [{ ad: "MOBMITT"}, ...body];
    articleBody.splice(ad1Position, 0, { ad: "MOBMITT" });
    // +1 needed because inserted ad1 changes positioning after it
    articleBody.splice(ad2Position+1, 0, { ad: "DIGIHELMOB" });
  } else if (ad1Position) {
    articleBody.splice(ad1Position, 0, { ad: "MOBMITT" });
  }

  return articleBody;
}
