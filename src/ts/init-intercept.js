"use strict";

const app = Elm.Intercept.init({
  node: document.getElementById("elm-app-is-loaded-here"),
  flags: { "href": location.href, "epoch": Date.now() },
});

browser.runtime.sendMessage({ tag: "request-model" });

browser.runtime.onMessage.addListener(receiveMessage);

function receiveMessage(message) {
  app.ports.receiveMessage.send(message);
}
