var inputFieldIdCounter = 0;

exports.generateIdNumber = function () {
  return inputFieldIdCounter++;
};
