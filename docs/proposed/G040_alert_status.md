# G040 — Alerts and status messages <!-- omit from toc -->

Source: `examples/reference-scenarios.md` (validation failure, asynchronous refresh)
Milestone: 1 — minimal loop (Web)

## Need

Feedback to the user arrives asynchronously: an error alert after a
failed action, a status message after a refresh. Both are live-region
semantics, and both appear without being summoned by a further
action: a scenario asserts that an alert or a status message
eventually appears, with its text.

## Vocabulary

- `an alert named <text> eventually appears`;
- `a status message <text> eventually appears`.

## Grounding

- [alert](https://www.w3.org/TR/wai-aria-1.2/#alert): assertive live
  region, appears without focus change;
  [status](https://www.w3.org/TR/wai-aria-1.2/#status): polite live
  region; both rely on
  [aria-live](https://www.w3.org/TR/wai-aria-1.2/#aria-live)
  semantics: the appearance itself is the event, which makes the
  `eventually` waiting of G025 event-driven rather than polled,
  where the platform allows it.
- Closes milestone 1: the four reference scenarios are the exit
  criterion.

## Open questions

- Q007, Q014: the backend contract must say whether an appearance is
  observed as an event or detected by polling.
