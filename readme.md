**Intentful provides a new approach to managing your attention and focus online.** It is a proof-of-concept browser extension, currently works in Firefox.

## How it Works

Mark web sites as unsafe if you're prone to using them compulsively, or getting sidetracked when you visit.

![Screenshot of unknown site](docs/assets/img/unknown-site.png)

When you try to load an unsafe site, the extension nudges you toward more intentful behavior.

![Screenshot of intercept page](docs/assets/img/intercept-page.png)

If you're still determined to proceed, you must 1. write why you're visiting the unsafe site, 2. set yourself a time limit, and 3. wait a few moments for any impulse to pass. Only then can you proceed to the unsafe site.

![Screenshot of unknown site](docs/assets/img/create-exception.png)

Once your time limit expires, Intentful encourages self-reflection and nudges you away from the unsafe site again.

![Screenshot of unknown site](docs/assets/img/time-expired.png)

## Why it Works

Intentful is a time-delay filter for your procrastinatory impulses. It does not block web sites, but _slows you down for a minute_, prompting you to reflect and consider your actions. This de-programs the instant dopamine hit that you receive when opening a social media or news site, but allows you to continue there when you have a genuine, pre-meditated need to use it.

There is a fundamental problem with tools that outright block web sites: _they also dare you to disable or remove the blocking tool_. Imagine that you're planning your weekend. You need to check the time of an event that's advertised only on a social media site, but you've already set up an app to block your own access to that site. In order to go there, you must disable or remove the blocking app. If you do, the safety railing is completely gone, leaving your vulnerable pre-frontal cortex to be tossed around the engagement-hacked web. We don't want that!

Think of Intentful as a safety railing that you can move aside temporarily, with some consideration and effort. Then it springs back into place afterward. The goal is to augment and build your self-control. Hopefully, the longer you use Intentful, the less you actually need it.

(Intentful is not yet proven to work empirically! It's designed around some educated hypotheses. Anecdotally, it works for the author. It could also work for you. If it doesn't, I'd like to hear your story. Send an email to intentful (at) cmart (period) today.)

## Privacy / Telemetry Pledge

Intentful will never send telemetry, analytics, or crash reports, "phone home", or do anything similar before obtaining the informed consent of you, the user. (It doesn't currently do any of this at all, but any future telemetry or crash reporting will be strictly opt-in.)

Further, Intentful does not fetch _any_ resources from third-party CDNs (such as Google Fonts) that can track your activity across the web. From a network connection perspective, it is completely self-contained once installed. The only connections that Intentful will make are those truly needed to provide the functionality you expect.

## How to Load Extension

For now:

- `git clone` the repository to your computer.
- In Firefox, browse to `about:debugging`.
- Click "This Firefox" in the left-hand navigation column.
- Click "Load Temporary Add-on...".
- Browse to `manifest.json` inside the `intentful` repo.

You must load the add-on each time you restart your browser. This is not great, will be better once I upload it to addons.mozilla.org.
 
## Future Plans

todo

## Test Mode

Enable test mode for development purposes. It reduces the waiting periods from ~30 seconds to ~3, so you can test changes quickly.

First, browser to `about:debugging#/runtime/this-firefox`, and inspect the extension:

![inspect extension screenshot](docs/assets/img/inspect-extension.jpg)

Then enter in the extension-specific developer console:

```js
browser.storage.local.set({mode: "testMode"});
```

Then reload the extension.

![reload extension screenshot](docs/assets/img/reload-extension.jpg)
