# G050 — MVP widget role set <!-- omit from toc -->

Source: ARIA 1.2 widget role taxonomy; roadmap phase 1
("freeze the MVP vocabulary subset")
Milestone: 2 — wider ARIA vocabulary (Web)

## Need

The MVP vocabulary must be an explicit, frozen subset of the ARIA
[widget
roles](https://www.w3.org/TR/wai-aria-1.2/#widget_roles), not an
open-ended "any role works": a documented, closed set of roles
supported at MVP, each with its name lookup, its states (G045), and
its actions (G020, G035). Everything else (tree, grid of widgets,
menu bar, and so on) is explicitly out of the MVP and listed as
future work.

## Proposed MVP set

| Role | States | Actions |
|---|---|---|
| `button` | enabled, pressed | press |
| `textbox` | enabled, required, invalid, readonly, multiline | type into |
| `checkbox` | checked (incl. mixed) | toggle/check/uncheck |
| `radio` (+ `radiogroup`) | checked, selected | select |
| `combobox` | expanded, collapsed | open, select option |
| `listbox` / `option` | selected | select |
| `slider` | value, orientation | set value |
| `spinbutton` | value | increment, decrement, set value |
| `tab` / `tabpanel` | selected | select tab |

## Design notes

- Directly implements roadmap phase 1, "freeze the MVP vocabulary
  subset"; the set is backend-independent, each backend declaring
  which roles it supports (capability report), and an out-of-set
  role failing with a clear "not in the MVP vocabulary" diagnostic.

## Open questions

- Q003 (which ARIA relations join the MVP).
