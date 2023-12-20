export function getCurrentCampaignNo_(paper) {
    var num = NaN;
    switch (paper) {
    case "HBL":
        num = parseInt(process.env.HBL_CAMPNO);
        break;
    case "ON":
        num = parseInt(process.env.ON_CAMPNO);
        break;
    case "VN":
        num = parseInt(process.env.VN_CAMPNO);
        break;
    }
    return isNaN(num) ? null : num;
}
