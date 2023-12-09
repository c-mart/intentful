import "./elm-background.js";

const backgroundApp = Elm.Background.init();

backgroundApp.ports.setRedirect.subscribe(function ({ tabId, url }) {
  browser.tabs.update(tabId, { url: url });
});

backgroundApp.ports.sendModel.subscribe(function (model) {
  browser.runtime.sendMessage({ model: model });
});

browser.webNavigation.onBeforeNavigate.addListener((details) => {
  backgroundApp.ports.getUrlChange.send({
    tabId: details.tabId,
    url: details.url,
  });

  return;
});

browser.runtime.onMessage.addListener(
  backgroundApp.ports.receiveModelRequest.send,
);
