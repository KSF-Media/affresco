exports.orderError = function(message) {
    return new OrderError(message);
};

exports.packageError = function(message) {
    return new PackageError(message);
};

class OrderError extends Error {
  constructor(message) {
    super(message);
    this.name = 'OrderError';
  }
}

class PackageError extends Error {
  constructor(message) {
    super(message);
    this.name = 'PackageError';
  }
}
