export function fetchAdImpl(contentUnit) {
  try {
    window.googletag.cmd.push(function () {
      if (window.definedSlots.includes(contentUnit)) {
	window.googletag
	  .pubads()
	  .getSlots()
	  .map((s) => {
	    if (s.getSlotElementId() === contentUnit) {
	      window.googletag.pubads().refresh([s]);
	      // console.log("Refreshing ad: " + s.getSlotElementId());
	    } else {
	      window.googletag.display(contentUnit);
	    }
	  });
      }
    });
  } catch (err) {}
};

export function getGamId(contentUnit) {
  try {
    const slots = window.innerWidth < 1020 ? window.adSlots.mobile : window.adSlots.desktop;
    const slot = slots.find((element) => contentUnit === element.targetId);
    if (typeof slot === "undefined") {
      return null;
    } else {
      return slot.gamId;
    }
  } catch (err) {
    return null;
  }
};

export function getIsLazy(contentUnit) {
  try {
    const slots = window.innerWidth < 1020 ? window.adSlots.mobile : window.adSlots.desktop;
    const slot = slots.find((element) => contentUnit === element.targetId);
    if (typeof slot === "undefined") {
      return null;
    } else {
      return slot.isLazy;
    }
  } catch (err) {
    return null;
  }
}

export function showConsentRevocationMessage() {
  window.googlefc && window.googlefc.showRevocationMessage();
  //toggle between consent/no consent to cookies
  if (document.hasConsent) {
    document.cookie='FCCDCF=[null,null,null,["CPeQ1MAPeQ1MAEsABBSVCdCgAAAAAG_AAApAAAAPigYgABAARAAoADIAHgAgABUAC4AHAAPAAtABkADQAHIAPoAiACLAEwATQAngBbAD9AIAAggBCACMAFCAKUAZQAzQBogDZAHcAP0AhABEQCJgEWAJEASkAwIBioDqAOqAdsBAoCNQFNgKsAWyAu8BeYDBAGMgPiAA.YAAACGgAAAA","1~","8C40ECBC-FBAA-47A5-AF95-0AB7C8B24306"],null,null,[]]'
    document.hasConsent = false
  } else {
    document.cookie = 'FCCDCF=[null,null,null,["CPeQ1MAPeQ1MAEsABBSVCdCoAP_AAG_AAApAImQOQAAgAIgAUABkADwAQAAkABUAC4AHAAPAAtABkADQAHIAPoAiACKAEmAJgAmgBPACoAFsAL4AfoBAAEIAIwAUIApQBggDKAGaANEAbIA7gB-gEIAIiARMAiwBIgCUgGBAMVAaYBp4DqAOqAdsA_QCBQEagKbAVYAtkBd4C8wGCAMZAfEA_kCJgHxQMQAAgAIgAUABkADwAQAAqABcADgAHgAWgAyABoADkAH0ARABFgCYAJoATwAtgB-gEAAQQAhABGAChAFKAMoAZoA0QBsgDuAH6AQgAiIBEwCLAEiAJSAYEAxUB1AHVAO2AgUBGoCmwFWALZAXeAvMBggDGQHxAAA.d5AACGgAAAA","1~2072.2074.70.89.1097","37B79C06-8922-44EB-84E1-9F51784C10DD"],null,null,[]]'
    document.hasConsent = true
  }
}
