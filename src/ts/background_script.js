import "./elm-background.js";

const backgroundApp = Elm.Background.init();

backgroundApp.ports.setRedirect.subscribe(function ({ tabId, url }) {
  browser.tabs.update(tabId, { url: url });
});

backgroundApp.ports.sendMessage.subscribe(function (message) {
  browser.runtime.sendMessage(message);
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
