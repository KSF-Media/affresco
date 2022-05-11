export function addHandler(element, handler, type) {
  if (type === undefined) {
    type = "click";
  }
  if (element._hasEventHandler) {
    element.removeEventListener
      ? element.removeEventListener(type, element._hasEventHandler)
      : element.detachEvent("on" + type, element._hasEventHandler);
  }
  element.addEventListener ? element.addEventListener(type, handler) : element.attachEvent("on" + type, handler);
  element._hasEventHandler = handler;
}
