"use strict";

let app;

browser.runtime.onMessage.addListener(receiveMessage);

function receiveMessage(message) {
    if (app === undefined && message.tag === "send-model") {

        app = Elm.BrowserAction.init({
            node: document.getElementById("elm-app-is-loaded-here"),
            flags: { "epoch": Date.now(), "common-model": message.model },
        });

        app.ports.sendMessage.subscribe(function (message) {
            browser.runtime.sendMessage(message);
        });

    } else if (app !== undefined) {

        app.ports.receiveMessage.send(message);

    }
}

browser.runtime.sendMessage({ tag: "request-model" });