exports.orderError = function(message) {
  return new OrderError(message);
};

exports.packageError = function(message) {
  return new PackageError(message);
};

exports.loginError = function(message) {
  return new LoginError(message);
};

exports.subscriptionError_ = function(message) {
  return new SubscriptionError(message);
};

exports.userError = function(message) {
  return new UserError(message);
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

class SubscriptionError extends Error {
  constructor(message) {
    super(message);
    this.name = 'SubscriptionError';
  }
}

class UserError extends Error {
  constructor(message) {
    super(message);
    this.name = 'UserError';
  }
}
