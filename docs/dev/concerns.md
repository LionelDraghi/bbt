# Concerns

This is a lightweight index, not a duplicate issue tracker. Each row
supplies priority, a short label, kind, authoritative link, and status;
the details remain in the authoritative source.

| Priority | Concern | Kind | Source | Status |
|---|---|---|---|---|
| (none open) | | | | |

## Priority definitions

- P1: blocks normal use, threatens correctness, breaks CI or release,
  or creates material risk;
- P2: important and actionable but not immediately blocking;
- P3: worthwhile maintenance or improvement with limited current impact.

Kinds: Bug, Debt, Design, Quality. The Source column points to the
authoritative place — an issue, a pull request, a proposal, a generated
report. A closed concern is removed or marked closed according to the
project's desired history policy.

The issues formerly indexed here (#2, #6, #9, #11, #15, #32, #37) are
all closed; each remains recorded where it is discussed, in the
feature pages and non-regression scenarios that cite it. The former
generated `issues_index.md` (a `make doc` scan of `Issue #` references)
is retired: grep `Issue #` over `docs/` and `src/` reproduces it on
demand.
