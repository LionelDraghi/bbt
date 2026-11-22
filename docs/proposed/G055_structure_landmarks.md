# G055 — Structure roles and landmarks <!-- omit from toc -->

Source: ARIA 1.2 document structure and landmark taxonomies
Milestone: 2 — wider ARIA vocabulary (Web)

## Need

Real applications are more than widgets: titles, lists, tables, and
regions. Structure roles serve two purposes: asserting structure,
and scoping other queries to a region.

## Vocabulary

- Structure: `heading` (with level), `list` / `listitem`,
  `table` / `row` / `cell` / `columnheader`;
- Landmarks: `main`, `navigation`, `banner`, `contentinfo`,
  `complementary`, `search`, and named `region`;
- Scoping: `in the <name> <role>` qualifier on existing assertions.

## Grounding

[Document structure
roles](https://www.w3.org/TR/wai-aria-1.2/#document_structure_roles)
and [landmark
roles](https://www.w3.org/TR/wai-aria-1.2/#landmark_roles) of the
ARIA taxonomy. Landmarks are the GUI vocabulary replacement for
XPath-ish scoping: a scenario scopes to `the main region`, never to
a DOM path.

## Design note

- First composite object: a region containing other objects — the
  canonical model must define containment and scoped lookup
  (phase 1).
