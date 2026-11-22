# G060 — Focus and keyboard navigation <!-- omit from toc -->

Source: ARIA 1.2 (focus management practices); roadmap phase 1
Milestone: 2 — wider ARIA vocabulary (Web)

## Need

`has focus` exists since G030 for a single widget. Real dialogs move
focus (initial focus, Tab order, focus traps, composite widgets
driven by arrows): scenarios must observe and assert this movement,
and assert keyboard navigation behaviour.

## Vocabulary

- `the focus is on the <name> <role>` (generalized form of G030);
- `When I press the key <key>`: single-key keyboard input, distinct
  from typing into a field (G035) and from a semantic press (G020);
- `the focus stays within the <name> dialog`: containment assertion.

## Grounding

Focus is platform state, not an ARIA property; ARIA governs what
should be focusable and composite-widget patterns such as
[aria-activedescendant](https://www.w3.org/TR/wai-aria-1.2/#aria-activedescendant)
(combobox, listbox). The backend observes focus events where the
platform provides them, polls otherwise.

## Design notes

- First keyboard input: this is where the action-tier model
  (semantic vs system input) becomes observable, and where CI
  profiles start to differ (see `docs/dev/ci-strategy.md`).

## Open questions

- Q008, Q014.
