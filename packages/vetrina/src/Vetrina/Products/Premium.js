export function getCurrentCampaignNo (paper) {
  switch (paper) {
    case "HBL":
      return process.env.HBL_CAMPNO;
    case "ÖN":
      return process.env.ON_CAMPNO;
    case "VN":
      return process.env.VN_CAMPNO;
  }
}
