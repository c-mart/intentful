"use strict";

let app;

async function getCurrentTabUrl() {
    try {
        const tabs = await browser.tabs.query({ active: true, currentWindow: true });
        const currentTab = tabs[0];
        return currentTab.url;
    } catch (error) {
        console.error("Failed to get current tab:", error);
    }
}

browser.runtime.onMessage.addListener(receiveMessage);

async function receiveMessage(message) {
    if (app === undefined && message.tag === "send-model") {

        const currentTabUrl = await getCurrentTabUrl();
        console.log(currentTabUrl);

        app = Elm.BrowserAction.init({
            node: document.getElementById("elm-app-is-loaded-here"),
            flags: {
                "epoch": Date.now(),
                "common-model": message.model,
                "currentTabUrl": currentTabUrl
            },
        });

        app.ports.sendMessage.subscribe(function (message) {
            browser.runtime.sendMessage(message);
        });

    } else if (app !== undefined) {

        app.ports.receiveMessage.send(message);

    }
}

// TODO send message with new tab URL when tab URL changes.

browser.runtime.sendMessage({ tag: "request-model" });