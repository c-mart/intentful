import "./elm-background.js";

browser.webNavigation.onBeforeNavigate.addListener(() => {
  // Doing this again synchronously because we cannot register an event listener from the async code below
  return;
});

let gettingStoredState = browser.storage.local.get();

gettingStoredState.then(onGot, onError);

function onGot(storedState) {
  const backgroundApp = Elm.Background.init({ flags: { "storedState": storedState } });

  // Per https://developer.mozilla.org/en-US/docs/Mozilla/Add-ons/WebExtensions/API/alarms/create, Chrome will override this to 1 minute, but Firefox supports sub-minute wake-up intervals.
  const periodInMinutes = 0.25;
  browser.alarms.create("wake-up", { periodInMinutes });
  browser.alarms.onAlarm.addListener(backgroundApp.ports.receiveAlarm.send);


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
      frameId: details.frameId
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

  function setTestMode() {
    backgroundApp.ports.setTestMode.send(Date.now());
  }
  window.setTestMode = setTestMode;

}


function onError(error) {
  console.log(`Error: ${error}`);
}

