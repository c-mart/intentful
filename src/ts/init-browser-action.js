"use strict";

const app = Elm.BrowserAction.init({
    node: document.getElementById("elm-app-is-loaded-here"),
    flags: {},
});

browser.runtime.sendMessage({ tag: "request-model" });

browser.runtime.onMessage.addListener(receiveMessage);

app.ports.sendMessage.subscribe(function (message) {
    browser.runtime.sendMessage(message);
});

function receiveMessage(message) {
    app.ports.receiveMessage.send(message);
}
