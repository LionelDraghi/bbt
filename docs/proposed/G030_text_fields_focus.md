# G030 — Text fields and focus <!-- omit from toc -->

Source: `examples/reference-scenarios.md` (continue to login, validation failure)
Milestone: 1 — minimal loop (Web)

## Need

Forms are the second most basic GUI pattern: text entry fields, and
the focus that says where the next input goes. A scenario asserts the
presence of a text field by name, distinguishes a password field, and
checks which control has focus.

## Vocabulary

- `the dialog contains a textbox named <name>`;
- `a password textbox named <name>` (naming under discussion);
- `the <name> textbox has focus`.

## Grounding

- Role [textbox](https://www.w3.org/TR/wai-aria-1.2/#textbox); a
  multiline field is the same role with
  [aria-multiline](https://www.w3.org/TR/wai-aria-1.2/#aria-multiline)
  — one role, one vocabulary entry.
- Password: there is no password role in ARIA; a password field is
  exposed as a text field with a protected state at the accessibility
  API level (concern Q022).
- Focus is not an ARIA concept: it comes from the platform focus
  state and events.

## Open questions

- Q022 (password field naming). First use of a state that changes
  over time without an action (focus): the observation model must
  distinguish static lookup from tracked state.
