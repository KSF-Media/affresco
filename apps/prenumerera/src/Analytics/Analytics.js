if (typeof window !== "undefined") {
  window.dataLayer = window.dataLayer || [];
}

export function _purchase(cusno, packageId, method, price) {
  const date = new Date();
  dataLayer.push({
    event: "purchase",
    ecommerce: {
      currency: "EUR",
      transaction_id: cusno + "." + packageId + "." + date.toISOString(),
      payment_type: method,
      value: price,
      items: [
        {
          item_name: packageId,
          price: price,
          quantity: 1,
        },
      ],
    },
  });
}
