# G070 — Region snapshots and visual assertions <!-- omit from toc -->

Source: `docs/proposed/candidate-features.md` (chess snapshots, comparison policies)
Milestone: 3 — beyond the reference scenarios (gated)

## Need

Some assertions are visual by nature: a chessboard square is empty,
a rendering matches a reference image. The candidate features ask
for region-scoped snapshots and configurable comparison rules:
under explicit policies, a scenario captures a semantic region and
compares it.

## Vocabulary

- `the snapshot of <target> is equal to <asset>`;
- comparison policies: scaling, antialiasing, colour tolerance.

## Grounding

None: deliberately outside the ARIA vocabulary. It must not compete
with semantic assertions — the candidate features require an
explicit distinction between semantic state assertions and visual
rendering assertions, and S003 forbids toolkit-dependent scenarios:
a visual assertion is anchored on a semantic region (a named
object), never on coordinates.

## Design notes

- New object kind in the canonical model: a visual attachment
  attached to a semantic object; ties into the protocol attachment
  design (concern Q018).

## Gate

Promote only after milestone 2: this feature risks dragging the
protocol into image management before the semantic core is stable.
