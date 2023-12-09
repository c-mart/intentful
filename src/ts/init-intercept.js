"use strict";

const app = Elm.Intercept.init({
  node: document.getElementById("elm-app-is-loaded-here"),
});

browser.runtime.sendMessage({ tag: "request-model" });

browser.runtime.onMessage.addListener(receiveMessage);

function receiveMessage(message) {
  app.ports.receiveModel.send(message.model);
}
