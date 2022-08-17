export function encodeURIComponent_(s) {
  return encodeURIComponent(s);
};

export const nativeShare = (typeof window !== "undefined" && window.navigator && window.navigator.share)
  ? (data) => {
      window.navigator.share(data);
    }
  : null;
