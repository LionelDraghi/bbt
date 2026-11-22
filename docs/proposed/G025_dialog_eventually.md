# G025 — Dialog appearance, with bounded waiting <!-- omit from toc -->

Source: `examples/reference-scenarios.md` (continue to login)
Milestone: 1 — minimal loop (Web)

## Need

GUI behaviour is asynchronous: a dialog appears after an action, not
instantaneously. The scenario language needs an explicit, bounded
wait: a scenario asserts that a dialog eventually appears, waiting
semantics explicit and bounded.

## Vocabulary

- `Then a <name> dialog eventually appears`: role
  [dialog](https://www.w3.org/TR/wai-aria-1.2/#dialog), awaited
  within a bounded deadline;
- `eventually` is the GUI waiting keyword: it introduces a deadline,
  it is not a fixed sleep; `alertdialog` is the same feature with the
  alert semantics of G040.

## Open questions

- Q013 to Q015: it introduces time into the model — deadline,
  polling or event-driven wait, per-step vs per-scenario deadline.
