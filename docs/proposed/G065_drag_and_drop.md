# G065 — Semantic drag-and-drop <!-- omit from toc -->

Source: `docs/proposed/candidate-features.md` (chess, TopoProof)
Milestone: 3 — beyond the reference scenarios

## Need

Drag-and-drop is a first-class GUI interaction in the candidate
features (chess moves, geometry manipulation), expressed with
domain-level objects and units, not pixels: a scenario expresses a
drag from a semantic source to a semantic target, optionally in
domain units.

## Vocabulary

- `When I drag the <name> <role> to the <name> <role>`;
- `When I drag the <name> <role> <distance> <unit> to the <direction>`
  (domain-unit variant, e.g. `1 cm to the left`);
- postcondition assertions on the result (`the piece is on
  <square>`, the snapshot checks belonging to G070).

## Grounding

ARIA has no drag-and-drop semantics — the deprecated
[aria-grabbed](https://www.w3.org/TR/wai-aria-1.2/#aria-grabbed) and
[aria-dropeffect](https://www.w3.org/TR/wai-aria-1.2/#aria-dropeffect)
are evidence that it deliberately does not. This is the first
deliberately non-ARIA part of the vocabulary: each backend maps it
to its native mechanism, and its feasibility differs per pilot
target — the capability report must state what drag support means
per backend.
