if (typeof window !== "undefined") {
  window.dataLayer = window.dataLayer || [];
}

exports.login_ = function (cusno, method, result) {
  dataLayer.push({ event: "login", cusno: cusno === null ? "" : cusno, method: method, result: result });
};

exports.reclamation_ = function (cusno, subsno, date, action, result) {
  dataLayer.push({ event: "reclamation", cusno: cusno, subsno: subsno, date: date, action: action, result: result });
};

exports.tempAddressChange_ = function (cusno, subsno, startDate, endDate, result) {
  dataLayer.push({
    event: "tempAddressChange",
    cusno: cusno,
    subsno: subsno,
    startDate: startDate,
    endDate: endDate,
    result: result,
  });
};

exports.editTempAddressChange_ = function (cusno, subsno, oldStartDate, startDate, endDate, result) {
  dataLayer.push({
    event: "tempAddressChange",
    cusno: cusno,
    subsno: subsno,
    oldStartDate: oldStartDate,
    startDate: startDate,
    endDate: endDate,
    result: result,
  });
};

exports.deleteTempAddressChange_ = function (cusno, subsno, startDate, endDate, result) {
  dataLayer.push({
    event: "deleteTempAdressChange",
    cusno: cusno,
    subsno: subsno,
    startDate: startDate,
    endDate: endDate,
    result: result,
  });
};

exports.pauseSubscription_ = function (cusno, subsno, startDate, endDate, result) {
  dataLayer.push({
    event: "pauseSubscription",
    cusno: cusno,
    subsno: subsno,
    startDate: startDate,
    endDate: endDate,
    result: result,
  });
};

exports.editSubscriptionPause_ = function (cusno, subsno, oldStartDate, oldEndDate, startDate, endDate, result) {
  dataLayer.push({
    event: "pauseSubscription",
    cusno: cusno,
    subsno: subsno,
    oldStartDate: startDate,
    oldEndDate: endDate,
    newStartDate: startDate,
    newEndDate: endDate,
    result: result,
  });
};

exports.unpauseSubscription_ = function (cusno, subsno, startDate, endDate, result) {
    dataLayer.push({
	event: "unPauseSubscription",
	cusno: cusno,
	subsno: subsno,
	startDate: startDate,
	endDate: endDate,
	result: result
    });
};

exports.changeName_ = function (cusno, result) {
  dataLayer.push({ event: "changeName", cusno: cusno, result: result });
};

exports.changeEmail_ = function (cusno, result) {
  dataLayer.push({ event: "changeEmail", cusno: cusno, result: result });
};

exports.changeAddress_ = function (cusno, result) {
  dataLayer.push({ event: "changeAddress", cusno: cusno, result: result });
};

exports.deletePendingAddressChanges_ = function (cusno, result) {
  dataLayer.push({ event: "deletePendingAddressChanges", cusno: cusno, result: result });
};

exports.updateResetPassword_ = function (result) {
  dataLayer.push({ event: "updateResetPassword", result: result });
};

exports.updateCreditCard_ = function (cusno, subsno, oldCreditCard, registerNumber, result) {
  dataLayer.push({
    event: "updateCreditCard",
    cusno: cusno,
    subsno: subsno,
    oldCreditCard: oldCreditCard,
    registerNumber: registerNumber,
    result: result,
  });
};
