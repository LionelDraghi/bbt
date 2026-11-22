# G090 — Spinbutton stepping <!-- omit from toc -->

Milestone: 2 — wider ARIA vocabulary

## Need

Same need as the slider (G085): valued widgets stay outside the
vocabulary. The spinbutton differs by one thing: its natural
interaction is a step, not an absolute set — a scenario should be
able to click the semantic increment.

## Vocabulary

- `When I increment the <name> spinbutton`;
- (negated/symmetric form to settle: `decrement`).

## Grounding

Role [spinbutton](https://www.w3.org/TR/wai-aria-1.2/#spinbutton);
same value properties as the slider (`aria-valuenow`, bounds). The
semantic increment is the widget's own stepping (its step size), not
a typed value: the scenario states an intent, the widget owns the
arithmetic.

## Design note

- The increment goes through the widget's AT-SPI action set on the
  gtk4 side (Gtk.Spin_Button), like a press (G020), not through a
  value set.

## Open questions

- A `decrement` variant? Same feature number (symmetric wording), or
  a separate vocabulary entry?

## Precondition

A valued-widget witness page (same precondition as G085).
