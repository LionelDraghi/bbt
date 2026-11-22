# Proposed Features <!-- omit from toc -->

This document is a lightweight inbox for candidate scenarios and feature ideas. Content here is exploratory: wording, semantics, and MVP inclusion are not yet decisions.

All scenario parameters follow `bbt` syntax: inline values are enclosed in backticks, while multiline text uses fenced code blocks.

## Contents <!-- omit from toc -->

- [Calculator](#calculator)
- [Chess](#chess)
- [TopoProof](#topoproof)
- [Cross-cutting feature candidates](#cross-cutting-feature-candidates)

## Calculator

```gherkin
- Given the display is `0`
- When I enter `2`
- And I press `+`
- And I enter `3`
- Then the display is `6`
```

This example raises several candidate features:

- controls addressed by their accessible value or label;
- semantic numeric entry;
- distinction between `press` and device-specific `click`;
- assertions on a display value.

> Open point: mathematically, entering `2`, pressing `+`, and entering `3` ordinarily produces `5`, not `6`. The expected value is preserved here as the submitted feature idea, but should be resolved before this becomes a conformance scenario.

## Chess

```gherkin
- When I select square `C2`
- And I drag it to square `E3`
- Then the snapshot of square `C2` is equal to `empty_case.svg`
- And the snapshot of square `E3` is equal to `Knight.svg`
```

This example raises several candidate features:

- application-domain objects such as chessboard squares;
- drag-and-drop as a semantic action with source and destination;
- image or snapshot comparison limited to a semantic region;
- asset-based visual assertions;
- configurable comparison rules for scaling, antialiasing, colour, and tolerance.

> The submitted example referred to snapshots of `C3` and `C2`, although the move was from `C2` to `E3`. The proposed wording uses `C2` and `E3`, which appears consistent with the described move.

## TopoProof

```gherkin
- When I select the leftmost point
- And I drag it `1 cm` to the left
- Then ...
```

This example raises several candidate features:

- semantic geometric selection such as `the leftmost point`;
- drag operations expressed in domain units rather than pixels;
- direction and signed displacement;
- mapping between model coordinates, world coordinates, and screen coordinates;
- postconditions on geometry, topology, constraints, or rendered state.

Possible future assertions include:

```gherkin
- Then the selected point is `1 cm` left of its previous position
- And the segment remains horizontal
- And the shape remains valid
```

These are placeholders for discussion, not accepted vocabulary.

## Cross-cutting feature candidates

- semantic drag-and-drop;
- domain-level targets and spatial selectors;
- unit-aware input and assertions;
- region-scoped snapshots;
- image comparison policies;
- explicit distinction between semantic state assertions and visual rendering assertions;
- recording whether a drag was performed through system input, toolkit input, semantic action, or direct manipulation.
