"use strict";

let elmAppInitialized = false;
// TODO refactor to combine these
let app;

browser.runtime.onMessage.addListener(receiveMessage);

function receiveMessage(message) {
  if (!elmAppInitialized && message.tag === "send-model") {

    app = Elm.Intercept.init({
      node: document.getElementById("elm-app-is-loaded-here"),
      flags: { "href": location.href, "epoch": Date.now(), "common-model": message.model },
    });

    app.ports.sendMessage.subscribe(function (message) {
      browser.runtime.sendMessage(message);
    });

    elmAppInitialized = true;

  } else if (elmAppInitialized) {

    app.ports.receiveMessage.send(message);

  }
}

browser.runtime.sendMessage({ tag: "request-model" });