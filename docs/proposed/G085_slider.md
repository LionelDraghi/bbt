# G085 — Slider value setting <!-- omit from toc -->

Milestone: 2 — wider ARIA vocabulary

## Need

Valued widgets (sliders, spinbuttons, progress indicators) are part
of the everyday toolkits; the vocabulary has no way to set a value
today. Setting it semantically — a value, never a mouse drag or key
sequence — keeps scenarios in the black box register of the GUI
vocabulary.

## Vocabulary

- `When I set the <name> slider to <value>`.

Reading a value back is covered by the widget content checks: same
numbering concern as G080, not re-decided here.

## Grounding

Role [slider](https://www.w3.org/TR/wai-aria-1.2/#slider); the value
is [aria-valuenow](https://www.w3.org/TR/wai-aria-1.2/#aria-valuenow)
(with `aria-valuemin`, `aria-valuemax`, `aria-orientation` as
context). The set is a semantic value change: exactly what
`aria-valuenow` exposes.

## Open questions

- Value matching: exact, or bounded by min/max steps?

## Precondition

A valued-widget witness page (the ToDoMVC app holds no slider; a
dedicated demo page or widget is required).
