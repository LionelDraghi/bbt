# G045 — Widget states <!-- omit from toc -->

Source: ARIA 1.2 states and properties; roadmap phase 1
Milestone: 2 — wider ARIA vocabulary (Web)

## Need

Beyond enabled/disabled, the state of a widget is what scenarios
assert: checked, expanded, selected, required, invalid — the states
of the ARIA ontology, and the natural second step after role and
name, expressed in user-visible terms.

## Vocabulary

- `is checked` / `is unchecked` / `is partially checked`;
- `is pressed` / `is not pressed`;
- `is expanded` / `is collapsed`;
- `is selected` / `is not selected`;
- `is required`, `is invalid`.

## Grounding

[aria-checked](https://www.w3.org/TR/wai-aria-1.2/#aria-checked)
(true/false/mixed),
[aria-pressed](https://www.w3.org/TR/wai-aria-1.2/#aria-pressed),
[aria-expanded](https://www.w3.org/TR/wai-aria-1.2/#aria-expanded),
[aria-selected](https://www.w3.org/TR/wai-aria-1.2/#aria-selected),
[aria-required](https://www.w3.org/TR/wai-aria-1.2/#aria-required),
[aria-invalid](https://www.w3.org/TR/wai-aria-1.2/#aria-invalid)
(disabled already in G015). Each vocabulary entry maps to exactly
one state property; no attribute name appears in a scenario.

## Design note

- Fixes the boolean-vs-string model of state values in the canonical
  object schema (phase 1).
