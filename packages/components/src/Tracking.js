if (typeof window !== "undefined") {
  window.dataLayer = window.dataLayer || [];
}

export function login_(cusno, method, result) {
  window.dataLayer.push({ event: "login", cusno: cusno === null ? "" : cusno, method: method, result: result });
}

export function reclamation_(cusno, subsno, date, action, result) {
  window.dataLayer.push({
    event: "reclamation",
    cusno: cusno,
    subsno: subsno,
    date: date,
    action: action,
    result: result,
  });
}

export function tempAddressChange_(cusno, subsno, startDate, endDate, result) {
  window.dataLayer.push({
    event: "tempAddressChange",
    cusno: cusno,
    subsno: subsno,
    startDate: startDate,
    endDate: endDate,
    result: result,
  });
}

export function editTempAddressChange_(cusno, subsno, oldStartDate, startDate, endDate, result) {
  window.dataLayer.push({
    event: "tempAddressChange",
    cusno: cusno,
    subsno: subsno,
    oldStartDate: oldStartDate,
    startDate: startDate,
    endDate: endDate,
    result: result,
  });
}

export function deleteTempAddressChange_(cusno, subsno, startDate, endDate, result) {
  window.dataLayer.push({
    event: "deleteTempAdressChange",
    cusno: cusno,
    subsno: subsno,
    startDate: startDate,
    endDate: endDate,
    result: result,
  });
}

export function pauseSubscription_(cusno, subsno, startDate, endDate, result) {
  window.dataLayer.push({
    event: "pauseSubscription",
    cusno: cusno,
    subsno: subsno,
    startDate: startDate,
    endDate: endDate,
    result: result,
  });
}

export function editSubscriptionPause_(cusno, subsno, oldStartDate, oldEndDate, startDate, endDate, result) {
  window.dataLayer.push({
    event: "pauseSubscription",
    cusno: cusno,
    subsno: subsno,
    oldStartDate: startDate,
    oldEndDate: endDate,
    newStartDate: startDate,
    newEndDate: endDate,
    result: result,
  });
}

export function unpauseSubscription_(cusno, subsno, result) {
  window.dataLayer.push({ event: "unPauseSubscription", cusno: cusno, subsno: subsno, result: result });
}

export function changeName_(cusno, result) {
  window.dataLayer.push({ event: "changeName", cusno: cusno, result: result });
}

export function changeEmail_(cusno, result) {
  window.dataLayer.push({ event: "changeEmail", cusno: cusno, result: result });
}

export function changeAddress_(cusno, result) {
  window.dataLayer.push({ event: "changeAddress", cusno: cusno, result: result });
}

export function changePhone_(cusno, result) {
  window.dataLayer.push({ event: "changePhone", cusno: cusno, result: result });
}

export function deletePendingAddressChanges_(cusno, result) {
  window.dataLayer.push({ event: "deletePendingAddressChanges", cusno: cusno, result: result });
}

export function updateResetPassword_(result) {
  window.dataLayer.push({ event: "updateResetPassword", result: result });
}

export function updateCreditCard_(cusno, subsno, oldCreditCard, registerNumber, result) {
  window.dataLayer.push({
    event: "updateCreditCard",
    cusno: cusno,
    subsno: subsno,
    oldCreditCard: oldCreditCard,
    registerNumber: registerNumber,
    result: result,
  });
}
