"use strict";

let app;

browser.runtime.onMessage.addListener(receiveMessage);

function receiveMessage(message) {
  if (app === undefined && message.tag === "send-model") {

    app = Elm.Intercept.init({
      node: document.getElementById("elm-app-is-loaded-here"),
      flags: { "href": location.href, "epoch": Date.now(), "common-model": message.model },
    });

    app.ports.sendMessage.subscribe(function (message) {
      browser.runtime.sendMessage(message);
    });

    app.ports.closeCurrentTab.subscribe(async function () {
      const currentTab = await browser.tabs.getCurrent();
      browser.tabs.remove(currentTab.id);
    })

  } else if (app !== undefined) {

    app.ports.receiveMessage.send(message);

  }
}

browser.runtime.sendMessage({ tag: "request-model" });