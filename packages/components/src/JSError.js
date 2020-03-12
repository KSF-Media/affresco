exports.orderError = function(message) {
    return new OrderError(message);
};

exports.packageError = function(message) {
    return new PackageError(message);
};

exports.loginError = function(message) {
    return new LoginError(message);
}

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

class LoginError extends Error {
  constructor(message) {
    super(message);
    this.name = 'LoginError';
  }
}
