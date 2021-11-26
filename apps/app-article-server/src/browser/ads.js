// This file is currently unused at 2021-11-26, but could be put back into use,
// or at least its logic might be useful as a reference,
// when adding the "ad in the middle of an article" (MOBMITT) ad slot back.
/* The purpose of this function is to place ad slots in page text. It will try to space them out avoiding images, news graphics, iframes etc.
    It will not place slots in articles with less than 4 paragraphs. If no good paragraphs are found, the slots are placed after the text. It will allow <b>, <a href> and <i> within paragraphs. Other tags will disqualify that paragraph. Note that this function will call the main ads script on completion. This should prevent timing issues, no ads are loaded till all paragraphs are rolled out and inspected.
    The script depends on the article text residing in an element with the id 'content'.
TODO: Could be done already server side
*/
function positionAdsWithinArticle() {
  let textParagraphNum = 0; // incremental number of text paragraphs
  let textParagraphsOK = []; // storage for list of good paragraphs. We will use this array to find good spots for our slots.
  let textParagraphCount = 0; // count of paragraph array groups
  let slotOne = '<div id="DIGIHELMOB"></div>';
  let slotTwo = '<div id="MOBMITT"></div>';
  textParagraphsOK.push([]);

  var contentDiv = document.getElementById("content");
  if (contentDiv != null) {
    contentDiv.childNodes.forEach((node) => {
      if (node.className === "html") {
        var OK = true;
        let approvedTags = ["B", "A", "I"];
        let tt = node.getElementsByTagName("*");
        // try to support iPads from 2013. They cannot iterate browser objects only js arrays.
        var t = Array.prototype.slice.call(tt);
        // check for non-text content
        if (t.length > 0) {
          for (let item of t) {
            let upperCased = item.nodeName;
            upperCased = upperCased.toUpperCase();

            if (approvedTags.indexOf(upperCased) > -1) {
            } else {
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
  textParagraphsOK = textParagraphsOK.filter((set) => set.length > 0);
  var digiHelPick = 0;
  var digiHelGood = false;
  var mobMittPick = 0;
  var mobMittGood = false;
  var AllParas = contentDiv.getElementsByClassName("html");
  if (AllParas && textParagraphsOK.length > 0) {
    digiHelPick = textParagraphsOK[0][0];
    if (digiHelPick > 3) {
      digiHelGood = true;
      AllParas[digiHelPick - 1].insertAdjacentHTML("beforebegin", slotOne);
    }
    let mMittSet = Math.ceil(textParagraphsOK.length / 2);
    if (mMittSet > 1 || textParagraphsOK[mMittSet - 1].length > 5) {
      mobMittPick = textParagraphsOK[mMittSet - 1][Math.floor(textParagraphsOK[mMittSet - 1].length / 2)];
      mobMittGood = true;
      AllParas[mobMittPick - 1].insertAdjacentHTML("beforebegin", slotTwo);
    }
    // Place slots after text in case we could not place them in text.
    if (!digiHelGood) {
      let endPlace = AllParas.length - 1;
      AllParas[endPlace].insertAdjacentHTML("afterend", slotOne);
    }
    if (!mobMittGood) {
      let endPlace = AllParas.length - 1;
      AllParas[endPlace].insertAdjacentHTML("afterend", slotTwo);
    }
  }

  if (window.ksfDfp) {
    window.ksfDfp.startUp();
  }
}

window.positionAdsWithinArticle = positionAdsWithinArticle;
