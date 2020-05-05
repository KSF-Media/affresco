window.onload = function () {
    "use strict";
    let ksfAdMover = {};
    ksfAdMover.checkPara = function (ptag, expectedType, expectedClass) {
        let response = false;
        expectedType = expectedType.toUpperCase(); // nodeName is always uppercase
        // check that the surrounding paragraphs are OK when placing the in text ad
        let pBefore = ptag.previousElementSibling;
        let pBeforeContent = pBefore.innerHTML.replace(/[^A-Za-z0-9]/g, '');
        let pBeforeClass = pBefore.classList.contains(expectedClass);
        let pSelf = ptag;
        let pSelfContent = pSelf.innerHTML.replace(/[^A-Za-z0-9]/g, '');
        let pSelfClass = pSelf.classList.contains(expectedClass);
        if ((!(/^\s*$/.test(pBeforeContent)) && !(/^\s*$/.test(pSelfContent))) && (pSelf.nodeName === expectedType && pBefore.nodeName === expectedType) && (pBeforeClass && pSelfClass)) {
            response = true;
        }
        return response;
    };

    /*
        Find ad slot for DIGIHELMOB and move it into the middle of the text,
        if available.
    */
    // this must be true. Setting it to false is the way to chicken out
    ksfAdMover.letTheBoxMove = true;
    // reduce risk of banner collisions and other placement oddities
    ksfAdMover.paragraphType = 'div'; // the type of HTML element used for paragraphs
    ksfAdMover.paragraphClass = 'html'; // the name of the css class of egible paragraphs
    ksfAdMover.articleContainerName = 'App'; // the class of the main article container. Could make sense to have this as id. But it is a class in the documents.
    ksfAdMover.articleTextContainerName = 'content'; // this is an id
    ksfAdMover.minimumTextLength = 5;
    ksfAdMover.adDivToMove = "DIGIHELMOB"; // name of the div that we should move up
    // text with less paragraphs are not touched

    ksfAdMover.textDivTop = window.document.getElementsByClassName(ksfAdMover.articleContainerName);
    if (!(ksfAdMover.textDivTop[0] === undefined)) {
        ksfAdMover.textDiv = document.getElementById(ksfAdMover.articleTextContainerName);

        // count the number of paragraphs
        ksfAdMover.paras = ksfAdMover.textDivTop[0].querySelectorAll(ksfAdMover.paragraphType + '.' + ksfAdMover.paragraphClass);
        ksfAdMover.parasNum = ksfAdMover.paras.length;
        if (ksfAdMover.parasNum < ksfAdMover.minimumTextLength) {
            // not worth moving ads in very short texts
            ksfAdMover.letTheBoxMove = false;
        }
        ksfAdMover.placementPositionNum = Math.floor(ksfAdMover.parasNum / 2);
        ksfAdMover.placementElement = ksfAdMover.paras[ksfAdMover.placementPositionNum];
        if (ksfAdMover.placementElement && ksfAdMover.letTheBoxMove) {
            // verify that the new placement is OK. This requires the new placement to be surrounded by non-empty text tags.
            let isParaOK = ksfAdMover.checkPara(ksfAdMover.placementElement, ksfAdMover.paragraphType, ksfAdMover.paragraphClass); // element + expected type and class name
            if (!isParaOK) {
                //last ditch attempt or give up
                ksfAdMover.placementElement = ksfAdMover.paras[ksfAdMover.placementPositionNum - 2];
                isParaOK = ksfAdMover.checkPara(ksfAdMover.placementElement, ksfAdMover.paragraphType, ksfAdMover.paragraphClass);
                // if still not OK, we chicken out
                if (!isParaOK) {
                    ksfAdMover.letTheBoxMove = false;
                }
            }
            ksfAdMover.adDivToMoveElement = window.document.getElementById(ksfAdMover.adDivToMove);
            if (ksfAdMover.adDivToMoveElement !== null) {
                if (ksfAdMover.letTheBoxMove) {
                    ksfAdMover.textDiv.insertBefore(ksfAdMover.adDivToMoveElement, ksfAdMover.placementElement);
                }
            } else {
                document.console.log('I was expecting an specific ad div to be here, it was not!');
            }
        }
        // found the necessary text containers
    } else {
        document.console.log('failed to find top container!');
    }
};