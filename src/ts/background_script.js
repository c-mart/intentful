import "./elm-background.js";

const backgroundApp = Elm.Background.init();

backgroundApp.ports.setRedirect.subscribe(function ({ tabId, url }) {
  browser.tabs.update(tabId, { url: url });
});

backgroundApp.ports.sendMessage.subscribe(function (message) {
  browser.runtime.sendMessage(message);
});

backgroundApp.ports.requestTabs.subscribe(async function () {
  const tabs = await browser.tabs.query({});
  backgroundApp.ports.receiveTabs.send(tabs);
});

browser.webNavigation.onBeforeNavigate.addListener((details) => {
  backgroundApp.ports.getUrlChange.send({
    tabId: details.tabId,
    url: details.url,
  });

  return;
});

browser.runtime.onMessage.addListener(
  backgroundApp.ports.receiveMessage.send,
);
