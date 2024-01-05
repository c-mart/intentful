import "./elm-background.js";

browser.webNavigation.onBeforeNavigate.addListener(() => {
  // Doing this again synchronously because we cannot register an event listener from the async code below
  return;
});

let gettingStoredState = browser.storage.local.get();

gettingStoredState.then(onGot, onError);

function onGot(storedState) {
  const backgroundApp = Elm.Background.init({ flags: { "storedState": storedState } });

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
      id: details.tabId,
      url: details.url,
    });

    return;
  });

  browser.runtime.onMessage.addListener(
    backgroundApp.ports.receiveMessage.send,
  );

  backgroundApp.ports.setStorage.subscribe(function (state) {
    browser.storage.local.set(state).then(null, null);
  });

  backgroundApp.ports.consoleLog.subscribe(function (e) {
    console.log(e);
  });
}

function onError(error) {
  console.log(`Error: ${error}`);
}

