"use strict";

let app;

async function getCurrentTab() {
    try {
        const tabs = await browser.tabs.query({ active: true, currentWindow: true });
        const currentTab = tabs[0];
        return currentTab;
    } catch (error) {
        console.error("Failed to get current tab:", error);
    }
}

browser.runtime.onMessage.addListener(receiveMessage);

async function receiveMessage(message) {
    if (app === undefined && message.tag === "send-model") {

        const currentTab = await getCurrentTab();

        app = Elm.BrowserAction.init({
            node: document.getElementById("elm-app-is-loaded-here"),
            flags: {
                "epoch": Date.now(),
                "common-model": message.model,
                "currentTabUrl": currentTab.url
            },
        });

        // Notify app when tab URL changes.
        browser.tabs.onUpdated.addListener(
            (tabId, changeInfo, tab) => {
                if (tabId === currentTab.id && changeInfo.url) {
                    console.log(`URL changed to: ${changeInfo.url}`);
                    app.ports.receiveCurrentTabUrl.send(changeInfo.url);
                }
            }, { properties: ["url"] }
        )

        app.ports.sendMessage.subscribe(function (message) {
            browser.runtime.sendMessage(message);
        });

    } else if (app !== undefined) {

        app.ports.receiveMessage.send(message);

    }
}

browser.runtime.sendMessage({ tag: "request-model" });