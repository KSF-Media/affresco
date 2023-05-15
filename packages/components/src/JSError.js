export function orderError(message) {
  return new OrderError(message);
}

export function packageError(message) {
  return new PackageError(message);
}

export function loginError(message) {
  return new LoginError(message);
}

export function subscriptionError_(message) {
  return new SubscriptionError(message);
}

export function userError(message) {
  return new UserError(message);
}

class OrderError extends Error {
  constructor(message) {
    super(message);
    this.name = "OrderError";
  }
}

class PackageError extends Error {
  constructor(message) {
    super(message);
    this.name = "PackageError";
  }
}

class LoginError extends Error {
  constructor(message) {
    super(message);
    this.name = "LoginError";
  }
}

class SubscriptionError extends Error {
  constructor(message) {
    super(message);
    this.name = "SubscriptionError";
  }
}

class UserError extends Error {
  constructor(message) {
    super(message);
    this.name = "UserError";
  }
}
